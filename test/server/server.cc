#include <uv.h>
#include <gtest/gtest.h>
#include <iostream>

#include "server/server.hh"

TEST(Server, BasicUVTest) {
  uv_loop_t *loop = (uv_loop_t*)malloc(sizeof(uv_loop_t));
  uv_loop_init(loop);

  uv_run(loop, UV_RUN_DEFAULT);

  uv_loop_close(loop);
  free(loop);
}

TEST(Server, ServerStartup) {
  runServer("localhost", 8080, 128);
}

TEST(Server, ServerConnection) {
  runServer("localhost", 8080, 128);

  uv_run(uv_default_loop(), UV_RUN_DEFAULT);
}

