#ifndef BREAD_DATA_DATA_HH_
#define BREAD_DATA_DATA_HH_

#include <vector>
#include <string>
#include <fstream>

// use 20 chars as the default allocation for space in the gap.
#define GAP_SPACE 20

/* 
   `BufferText` is the basic structure for storing file contents in
   Bread. It is an implementation of the traditional gap buffer using
   C++ vectors.
*/
struct BufferText {
  std::vector<char> before;
  std::vector<char> gap;
  std::vector<char> after;

  std::string flatten();
};


/* 
   `Buffer` is the class used to represent a buffer in its entirety.
   There should never be a duplicate buffer for a single file.
 */
class Buffer {
public:
  BufferText text;
  int cursor_pos;

  Buffer();

  bool setFile(std::string);
  
  std::string getFilename();
  
  bool read();
  bool write();

  

private:
  std::string filename;
  std::fstream file;
};

#endif
