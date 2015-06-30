#include <gtest/gtest.h>
#include <data/data.hh>

TEST(BufferTest, FlattenWorks) {
  std::string hello = "Hello!!";

  Buffer test1;
  test1.text.before.push_back('H');
  test1.text.before.push_back('e');
  test1.text.gap.push_back('l');
  test1.text.gap.push_back('l');
  test1.text.gap.push_back('o');
  test1.text.after.push_back('!');
  test1.text.after.push_back('!');

  Buffer test2;
  test2.text.before.push_back('H');
  test2.text.gap.push_back('e');
  test2.text.gap.push_back('l');
  test2.text.after.push_back('l');
  test2.text.after.push_back('o');
  test2.text.after.push_back('!');
  test2.text.after.push_back('!');
    
  ASSERT_STREQ(hello.c_str(), test1.text.flatten().c_str());
  ASSERT_STREQ(hello.c_str(), test2.text.flatten().c_str());
  ASSERT_STREQ(test1.text.flatten().c_str(), test2.text.flatten().c_str());
}

TEST(BufferTest, GapSizeSet) {
  Buffer test;

  ASSERT_EQ(test.text.gap.capacity(), GAP_SPACE);
}

TEST(BufferTest, ReadWrite) {
  Buffer test;
  test.setFile("./bin/Testfile.txt");
  
  ASSERT_EQ(true, test.read());
  ASSERT_STREQ("Hello world!\n", test.text.flatten().c_str());

  test.text.after.push_back('C');
  test.text.after.push_back('o');
  test.text.after.push_back('o');
  test.text.after.push_back('l');
  test.text.after.push_back('\n');

  ASSERT_EQ(true, test.write());
  ASSERT_STREQ("Hello world!\nCool\n", test.text.flatten().c_str());
  
  ASSERT_EQ(true, test.read());
  ASSERT_STREQ("Hello world!\nCool\n", test.text.flatten().c_str());

  for(int i = 0; i < 5; ++i) {
    test.text.after.pop_back();
  }
  
  ASSERT_EQ(true, test.write());
  ASSERT_STREQ("Hello world!\n", test.text.flatten().c_str());
  
  ASSERT_EQ(true, test.read());
  ASSERT_STREQ("Hello world!\n", test.text.flatten().c_str());
}
