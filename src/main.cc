#include <iostream>
#include <server/server.hh>

int main(int argc, char** argv) {
  runServer("localhost", 8080, 128);

  uv_run(uv_default_loop(), UV_RUN_DEFAULT);
}

