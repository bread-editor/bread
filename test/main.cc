#include <gtest/gtest.h>

TEST(ExampleTest, Basic) {
  ASSERT_EQ(8, 8);
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
