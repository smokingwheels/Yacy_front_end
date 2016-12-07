REM Have a play...Contact @smokingwheels....May the source be with you  #Linux...
REM change line 253 in html format to you public Yacy address..
REM http://www.qb64.net/ IDE Compiler Download Address.
REM Dec 7 2016
DEFINT A-Z
CONST MAX_CLIENTS = 8
CONST EXPIRY_TIME = 240 'seconds
CONST MIDNIGHT_FIX_WINDOW = 60 * 60 'seconds
CONST MAX_HEADER_SIZE = 4096 'bytes
CONST DEFAULT_HOST = "10.0.0.150"



CONST METHOD_HEAD = 1
CONST METHOD_GET = 2
CONST METHOD_POST = 3
DIM SHARED CRLF AS STRING
CRLF = CHR$(13) + CHR$(10)


'QB doesn't support variable-length strings in TYPEs :(
'This is sooooo ugly
'Important ones first
DIM client_handle(1 TO MAX_CLIENTS) AS INTEGER
DIM client_expiry(1 TO MAX_CLIENTS) AS DOUBLE
DIM client_request(1 TO MAX_CLIENTS) AS STRING
DIM client_uri(1 TO MAX_CLIENTS) AS STRING
DIM client_method(1 TO MAX_CLIENTS) AS INTEGER
DIM client_content_length(1 TO MAX_CLIENTS) AS LONG

'These ones are less important
DIM client_host(1 TO MAX_CLIENTS) AS STRING
DIM client_browser(1 TO MAX_CLIENTS) AS STRING
DIM client_content_encoding(1 TO MAX_CLIENTS) AS INTEGER


connections = 0
host = _OPENHOST("TCP/IP:8080")
DO
    'Process old connections
    IF connections THEN
        FOR c = 1 TO MAX_CLIENTS
            IF client_handle(c) THEN
                'work on the request in an effort to finish it
                IF try_complete_request(c) THEN
                    PRINT "Completed request for: " + client_uri(c)
                    PRINT " from " + _CONNECTIONADDRESS(client_handle(c))
                    PRINT " using " + client_browser(c)
                    tear_down c
                    connections = connections - 1
                    'check for expiry
                ELSEIF TIMER >= client_expiry(c) AND TIMER < client_expiry(c) + MIDNIGHT_FIX_WINDOW THEN
                    PRINT "TIMED OUT: request for: " + client_uri(c)
                    PRINT " from " + _CONNECTIONADDRESS(client_handle(c))
                    PRINT " using " + client_browser(c)
                    respond c, "HTTP/1.1 408 Request Timeout", ""
                    tear_down c
                    connections = connections - 1
                END IF
            END IF
        NEXT
    END IF
    'Accept any new connections
    IF connections < MAX_CLIENTS THEN
        newclient = _OPENCONNECTION(host) ' monitor host connection
        DO WHILE newclient
            FOR c = 1 TO MAX_CLIENTS
                IF client_handle(c) = 0 THEN
                    client_handle(c) = newclient
                    client_method(c) = 0
                    client_content_length(c) = -1
                    client_expiry(c) = TIMER(.001) + EXPIRY_TIME
                    IF client_expiry(c) >= 86400 THEN client_expiry(c) = client_expiry(c) - 86400
                    EXIT FOR
                END IF
            NEXT
            connections = connections + 1
            IF connections >= MAX_CLIENTS THEN EXIT DO
            newclient = _OPENCONNECTION(host) ' monitor host connection
        LOOP
    END IF
    'Limit CPU usage and leave some time for stuff be sent across the network..I have it as high as 1000 on my Front End
    _LIMIT 50

LOOP UNTIL INKEY$ <> "" ' any keypress quits
CLOSE #host
SYSTEM

SUB tear_down (c AS INTEGER)
SHARED client_handle() AS INTEGER, client_uri() AS STRING
SHARED client_host() AS STRING, client_browser() AS STRING
SHARED client_request() AS STRING

CLOSE #client_handle(c)
'set handle to 0 so we know it's unused
client_handle(c) = 0
'set strings to empty to save memory
client_uri(c) = ""
client_host(c) = ""
client_browser(c) = ""
client_request(c) = ""

END SUB

FUNCTION try_complete_request% (c AS INTEGER)
SHARED client_handle() AS INTEGER, client_uri() AS STRING
SHARED client_host() AS STRING, client_browser() AS STRING
SHARED client_content_length() AS LONG
SHARED client_request() AS STRING, client_method() AS INTEGER

'Apparently QB64 doesn't support this yet
'ON LOCAL ERROR GOTO runtime_internal_error
DIM cur_line AS STRING

GET #client_handle(c), , s$
IF LEN(s$) = 0 THEN EXIT FUNCTION
'client_request is used to collect the client's request
'when all the headers have arrived, they are stripped away from client_request
client_request(c) = client_request(c) + s$

IF client_method(c) = 0 THEN
    header_end = INSTR(client_request(c), CRLF + CRLF)
    IF header_end = 0 THEN
        IF LEN(client_request(c)) > MAX_HEADER_SIZE THEN GOTO large_request
        EXIT FUNCTION
    END IF

    'HTTP permits the use of multiple spaces/tabs and in some cases newlines
    'to separate words. So we collapse them.
    headers$ = shrinkspace(LEFT$(client_request(c), header_end + 1))
    client_request(c) = MID$(client_request(c), header_end + 4)

    'This loop processes all the header lines
    first_line = 1
    DO
        linebreak = INSTR(headers$, CRLF)
        IF linebreak = 0 THEN EXIT DO

        cur_line = LEFT$(headers$, linebreak - 1)
        headers$ = MID$(headers$, linebreak + 2)

        IF first_line THEN
            'First line looks something like
            'GET /index.html HTTP/1.1
            first_line = 0
            space = INSTR(cur_line, " ")
            IF space = 0 THEN GOTO bad_request
            method$ = LEFT$(cur_line, space - 1)
            space2 = INSTR(space + 1, cur_line, " ")
            IF space2 = 0 THEN GOTO bad_request
            client_uri(c) = MID$(cur_line, space + 1, space2 - (space + 1))
            IF LEN(client_uri(c)) = 0 THEN GOTO bad_request
            version$ = MID$(cur_line, space2 + 1)
            SELECT CASE method$
                CASE "GET"
                    client_method(c) = METHOD_GET
                CASE "HEAD"
                    client_method(c) = METHOD_HEAD
                CASE "POST"
                    client_method(c) = METHOD_POST
                CASE ELSE
                    GOTO unimplemented
            END SELECT
            SELECT CASE version$
                CASE "HTTP/1.1"
                CASE "HTTP/1.0"
                CASE ELSE
                    GOTO bad_request
            END SELECT
        ELSE
            'These are of the form "Name: Value", e.g.
            'Host: www.qb64.net

            colon = INSTR(cur_line, ": ")
            IF colon = 0 THEN GOTO bad_request
            header$ = LCASE$(LEFT$(cur_line, colon - 1))
            value$ = MID$(cur_line, colon + 2)
            SELECT CASE header$
                CASE "cache-control"
                CASE "connection"
                CASE "date"
                CASE "pragma"
                CASE "trailer"
                CASE "transfer-encoding"
                    GOTO unimplemented
                CASE "upgrade"
                CASE "via"
                CASE "warning"

                CASE "accept"
                CASE "accept-charset"
                CASE "accept-encoding"
                CASE "accept-language"
                CASE "authorization"
                CASE "expect"
                CASE "from"
                CASE "host"
                    client_host(c) = value$
                CASE "if-match"
                CASE "if-modified-since"
                CASE "if-none-match"
                CASE "if-range"
                CASE "if-unmodified-Since"
                CASE "max-forwards"
                CASE "proxy-authorization"
                CASE "range"
                CASE "referer"
                CASE "te"
                CASE "user-agent"
                    client_browser(c) = value$

                CASE "allow"
                CASE "content-encoding"
                    IF LCASE$(value$) <> "identity" THEN GOTO unimplemented
                CASE "content-language"
                CASE "content-length"
                    IF LEN(value$) <= 6 THEN
                        client_content_length(c) = VAL(value$)
                    ELSE
                        GOTO large_request
                    END IF
                CASE "content-location"
                CASE "content-md5"
                CASE "content-range"
                CASE "content-type"
                CASE "expires"
                CASE "last-modified"

                CASE ELSE

            END SELECT
        END IF

    LOOP
    'All modern clients send a hostname, so this is mainly to prevent
    'ancient clients and bad requests from tripping us up
    IF LEN(client_host(c)) = 0 THEN client_host(c) = DEFAULT_HOST
END IF

'assume the request can be completed; set to 0 if it can't.
try_complete_request = 1
htmlstart$ = "<html><head></head><body>You requested<br /><tt>"
SELECT CASE client_method(c)
    CASE METHOD_HEAD
        respond c, "HTTP/1.1 200 OK", ""
    CASE METHOD_GET
        'Say something interesting
        html$ = "<html><head></head><body>You requested nothing <tt>"
        '        html$ = html$ + "<iframe src=" + CHR$(34) + "http://sw.remote.mx/" + CHR$(34) + " style=" + CHR$(34) + "border:1px  solid" + CHR$(59) + CHR$(34) + " name=" + CHR$(34) + "Street" + CHR$(34) + " scroling=" + CHR$(34) + "auto" + CHR$(34) + " frameborder=" + CHR$(34) + "yes" + " align=" + CHR$(34) + "center" + CHR$(34) + " height = " + CHR$(34) + "100%" + CHR$(34) + " width = " + CHR$(34) + "100%" + CHR$(34) + ">" + "</iframe>"

        html$ = html$ + client_uri(c) + "</tt><form action='/' method='post'>"
        REM change address to suit your server and needs also can be an Android APP.
        html$ = html$ + "<iframe src=" + CHR$(34) + "http://sw.remote.mx:8095/index.html" + CHR$(34) + " style=" + CHR$(34) + "border:1px  solid" + CHR$(59) + CHR$(34) + " name=" + CHR$(34) + "Street" + CHR$(34) + " scroling=" + CHR$(34) + "auto" + CHR$(34) + " frameborder=" + CHR$(34) + "yes" + " align=" + CHR$(34) + "center" + CHR$(34) + " height = " + CHR$(34) + "100%" + CHR$(34) + " width = " + CHR$(34) + "100%" + CHR$(34) + ">" + "</iframe>"


        html$ = html$ + "<input type='text' name='var1' value='val1' />"
        html$ = html$ + "<input type='text' name='var2' value='val2' />"
        html$ = html$ + "<input type='submit' value='send a POST query'>"
        html$ = html$ + "</form></body></html>" + CRLF

        respond c, "HTTP/1.1 200 OK", html$

    CASE METHOD_POST
        IF LEN(client_request(c)) < client_content_length(c) THEN
            'message hasn't arrived yet or client disconnected
            try_complete_request = 0
        ELSE
            'Say something interesting
            html$ = "<html><head></head><body>You requested<br /><tt>"
            html$ = html$ + client_uri(c) + "</tt><br />and posted<br /><tt>"
            html$ = html$ + client_request(c) + "</tt><form action='/' method='get'>"
            html$ = html$ + "<input type='text' name='var1' value='val1' />"
            html$ = html$ + "<input type='text' name='var2' value='val2' />"
            html$ = html$ + "<input type='submit' value='send a GET query'>"
            html$ = html$ + "</form></body></html>" + CRLF

            respond c, "HTTP/1.1 200 OK", html$

        END IF
    CASE ELSE
        'This shouldn't happen because we would have EXITed FUNCTION earlier
        PRINT "ERROR: Unknown method. This should never happen."
END SELECT


EXIT FUNCTION


large_request:
respond c, "HTTP/1.1 413 Request Entity Too Large", ""
try_complete_request = 1
EXIT FUNCTION
bad_request:
respond c, "HTTP/1.1 400 Bad Request", ""
try_complete_request = 1
EXIT FUNCTION
unimplemented:
respond c, "HTTP/1.1 501 Not Implemented", ""
try_complete_request = 1
EXIT FUNCTION

runtime_internal_error:
PRINT "RUNTIME ERROR: Error code"; ERR; ", Line"; _ERRORLINE
RESUME internal_error
internal_error:
respond c, "HTTP/1.1 500 Internal Server Error", ""
try_complete_request = 1
EXIT FUNCTION


END FUNCTION

SUB respond (c AS INTEGER, header AS STRING, payload AS STRING)
SHARED client_handle() AS INTEGER
out$ = header + CRLF

out$ = out$ + "Date: " + datetime + CRLF
out$ = out$ + "Server: QweB64" + CRLF
out$ = out$ + "Last-Modified: " + datetime + CRLF
out$ = out$ + "Connection: close" + CRLF
'out$ = out$ + "Keep-Alive: timeout=15, max=99" + CRLF
'out$ = out$ + "Connection: Keep-Alive" + CRLF
IF LEN(payload) THEN
    out$ = out$ + "Content-Type: text/html; charset=UTF-8" + CRLF
    'out$ = out$ + "Transfer-Encoding: chunked" + CRLF
    out$ = out$ + "Content-Length:" + STR$(LEN(payload)) + CRLF
END IF

'extra newline to signify end of header
out$ = out$ + CRLF
PUT #client_handle(c), , out$

PUT #client_handle(c), , payload

END SUB

FUNCTION datetime$ ()
STATIC init AS INTEGER
STATIC day() AS STRING, month() AS STRING, monthtbl() AS INTEGER
IF init = 0 THEN
    init = 1
    REDIM day(0 TO 6) AS STRING
    REDIM month(0 TO 11) AS STRING
    REDIM monthtbl(0 TO 11) AS INTEGER
    day(0) = "Sun": day(1) = "Mon": day(2) = "Tue"
    day(3) = "Wed": day(4) = "Thu": day(5) = "Fri"
    day(6) = "Sat"
    month(0) = "Jan": month(1) = "Feb": month(2) = "Mar"
    month(3) = "Apr": month(4) = "May": month(5) = "Jun"
    month(6) = "Jul": month(7) = "Aug": month(8) = "Sep"
    month(9) = "Oct": month(10) = "Nov": month(11) = "Dec"
    'Source: Wikipedia
    monthtbl(0) = 0: monthtbl(1) = 3: monthtbl(2) = 3
    monthtbl(3) = 6: monthtbl(4) = 1: monthtbl(5) = 4
    monthtbl(6) = 6: monthtbl(7) = 2: monthtbl(8) = 5
    monthtbl(9) = 0: monthtbl(10) = 3: monthtbl(11) = 5
END IF
temp$ = DATE$ + " " + TIME$
m = VAL(LEFT$(temp$, 2))
d = VAL(MID$(temp$, 4, 2))
y = VAL(MID$(temp$, 7, 4))
c = 2 * (3 - (y \ 100) MOD 4)
y2 = y MOD 100
y2 = y2 + y2 \ 4
m2 = monthtbl(m - 1)
weekday = c + y2 + m2 + d

'leap year and Jan/Feb
IF ((y MOD 4 = 0) AND (y MOD 100 <> 0) OR (y MOD 400 = 0)) AND m <= 2 THEN weekday = weekday - 1

weekday = weekday MOD 7

datetime$ = day(weekday) + ", " + LEFT$(temp$, 2) + " " + month(m - 1) + " " + MID$(temp$, 7) + " GMT"

END FUNCTION

FUNCTION shrinkspace$ (str1 AS STRING)
DO
    i = INSTR(str1, CHR$(9))
    IF i = 0 THEN EXIT DO
    MID$(str1, i, 1) = " "
LOOP
DO
    i = INSTR(str1, CRLF + " ")
    IF i = 0 THEN EXIT DO
    str1 = LEFT$(str1, i - 1) + MID$(str1, i + 2)
LOOP
DO
    i = INSTR(str1, "  ")
    IF i = 0 THEN EXIT DO
    str1 = LEFT$(str1, i - 1) + MID$(str1, i + 1)
LOOP
shrinkspace = str1
END FUNCTION


