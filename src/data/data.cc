#include "data.hh"
#include <stdio.h>
#include <iostream>
#include <cstring>

/* BufferText */
std::string BufferText::flatten() {
  return std::string(before.begin(), before.end())
       + std::string(gap.begin(), gap.end())
       + std::string(after.begin(), after.end());
};

/* Buffer */
Buffer::Buffer() {
  cursor_pos = 0;

  text.gap.reserve(GAP_SPACE);
};

bool Buffer::setFile(std::string _filename) {
  file.open(_filename.c_str());

  bool good = file.good();
  file.close();
  
  if (good) {
    filename = _filename;
    return true;
  } else {
    return false;
  }
};

std::string Buffer::getFilename() {
  return filename;
}

bool Buffer::read() {
  std::string contents;
  file.open(filename.c_str());
  
  if (file) {
    file.seekg(0, std::ios::end);
    contents.resize(file.tellg());
    file.seekg(0, std::ios::beg);
    file.read(&contents[0], contents.size());
    file.close();

    text.after = std::vector<char>(contents.begin(), contents.end());

    return true;
  }

  return false;
}

bool Buffer::write() {
  file.open(filename.c_str(), std::ios::out);

  if (file.is_open()) {
    const char* buffer = text.flatten().c_str();
    file.write(buffer, strlen(buffer));
    bool status = file.fail();

    file.close();
      
    if (status) {
      return false;
    }
    
    return true;
  }
  
  return false;
}
