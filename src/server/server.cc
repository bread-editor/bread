#include "server.hh"
#include <iostream>

int runServer(const char *host, int port,
	      int backlog_amount = DEFAULT_BACKLOG_AMOUNT) {
  auto loop = uv_default_loop();
  sockaddr_in addr;

  uv_tcp_t server;
  uv_tcp_init(loop, &server);
  uv_ip4_addr(host, port, &addr);
  uv_tcp_bind(&server, (const struct sockaddr*)&addr, 0);

  std::cout << "Starting listen...\n";
  uv_listen((uv_stream_t*) &server, backlog_amount, onConnect);

  return 1;
}

void onConnect(uv_stream_t *server, int status) {
  if (status < 0) {
    std::cerr << "Error on new connection: " << uv_strerror(status) << "\n";
    return;
  }

  auto loop = uv_default_loop();

  uv_tcp_t *client = new uv_tcp_t;
  uv_tcp_init(loop, client);
  
  if(uv_accept(server, (uv_stream_t*) client) == 0) {
    uv_read_start((uv_stream_t*) client, allocBuffer, echoRead);
  } else {
    uv_close((uv_handle_t*) client, NULL);
  }
}

void allocBuffer(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf) {
  buf->base = (char*)malloc(suggested_size);
  
  if(buf->base == NULL) {
    std::cerr << "Problem allocating buffer space.\n";
    return;
  }
  
  buf->len = suggested_size;
}

void echoRead(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf) {
  uv_shutdown_t *req = new uv_shutdown_t;
  if (nread < 0) {
    if (nread != UV_EOF) {
      std::cerr << "Error: " << uv_strerror(nread);
    }

    uv_close((uv_handle_t*) client, NULL);
  } else if (nread > 0) {
    uv_write_t *req = new uv_write_t;
    uv_buf_t wrbuf = uv_buf_init(buf->base, nread);
    uv_write(req, client, &wrbuf, 1, echoWrite);
  }

  if (buf->base) {
    free(buf->base);
  }
}

void echoWrite(uv_write_t *req, int status) {
  if (status) {
    std::cerr << "Write error: " << uv_strerror(status);
  }

  free(req);
}
