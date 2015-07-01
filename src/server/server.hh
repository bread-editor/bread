#ifndef _BREAD_SERVER_SERVER
#define _BREAD_SERVER_SERVER
#include <uv.h>

#define DEFAULT_BACKLOG_AMOUNT 128

int runServer(const char *host, int port, int backlog_amount);
void onConnect(uv_stream_t *server, int status);
void allocBuffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf);
void echoRead(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf);
void echoWrite(uv_write_t *req, int status);

#endif
