#include <gtest/gtest.h>
#include "TrackingStream.hpp"
#include <sstream>
#include <string>

using namespace flynt;

class TrackingStreamTest : public ::testing::Test {
protected:
    void SetUp() override {}
    void TearDown() override {}
};

TEST_F(TrackingStreamTest, InitialPosition) {
    std::istringstream iss("test");
    TrackingStream ts(iss);

    EXPECT_EQ(ts.line(), 0);
    EXPECT_EQ(ts.column(), 0);
}

TEST_F(TrackingStreamTest, ReadSingleCharacter) {
    std::istringstream iss("a");
    TrackingStream ts(iss);

    char c;
    ts.get(c);
    EXPECT_EQ(c, 'a');
    EXPECT_EQ(ts.line(), 0);
    EXPECT_EQ(ts.column(), 1);
}

TEST_F(TrackingStreamTest, ReadMultipleCharacters) {
    std::istringstream iss("abc");
    TrackingStream ts(iss);

    char c;
    ts.get(c);
    EXPECT_EQ(c, 'a');
    EXPECT_EQ(ts.column(), 1);

    ts.get(c);
    EXPECT_EQ(c, 'b');
    EXPECT_EQ(ts.column(), 2);

    ts.get(c);
    EXPECT_EQ(c, 'c');
    EXPECT_EQ(ts.column(), 3);
}

TEST_F(TrackingStreamTest, PeekDoesNotAdvance) {
    std::istringstream iss("ab");
    TrackingStream ts(iss);

    EXPECT_EQ(ts.peek(), 'a');
    EXPECT_EQ(ts.line(), 0);
    EXPECT_EQ(ts.column(), 0);

    EXPECT_EQ(ts.peek(), 'a');
    EXPECT_EQ(ts.column(), 0);
}

TEST_F(TrackingStreamTest, GetAdvances) {
    std::istringstream iss("ab");
    TrackingStream ts(iss);

    EXPECT_EQ(ts.get(), 'a');
    EXPECT_EQ(ts.column(), 1);

    EXPECT_EQ(ts.get(), 'b');
    EXPECT_EQ(ts.column(), 2);
}

TEST_F(TrackingStreamTest, NewlineTracking) {
    std::istringstream iss("a\nb\nc");
    TrackingStream ts(iss);

    ts.get();
    EXPECT_EQ(ts.line(), 0);

    ts.get(); // '\n'
    EXPECT_EQ(ts.line(), 1);
    EXPECT_EQ(ts.column(), 0);

    ts.get(); // 'b'
    EXPECT_EQ(ts.line(), 1);
    EXPECT_EQ(ts.column(), 1);

    ts.get(); // '\n'
    EXPECT_EQ(ts.line(), 2);
    EXPECT_EQ(ts.column(), 0);
}

TEST_F(TrackingStreamTest, UngetSingleCharacter) {
    std::istringstream iss("abc");
    TrackingStream ts(iss);

    ts.get();
    ts.get();
    EXPECT_EQ(ts.column(), 2);

    ts.unget();
    EXPECT_EQ(ts.column(), 1);

    EXPECT_EQ(ts.get(), 'b');
}

TEST_F(TrackingStreamTest, PutbackCharacter) {
    std::istringstream iss("abc");
    TrackingStream ts(iss);

    char c;
    ts.get(c);
    ts.get(c);
    EXPECT_EQ(ts.column(), 2);

    ts.putback('b');
    EXPECT_EQ(ts.column(), 1);

    ts.get(c);
    EXPECT_EQ(c, 'b');
}

TEST_F(TrackingStreamTest, ReadWithBuffer) {
    std::istringstream iss("hello world");
    TrackingStream ts(iss);

    char buffer[6];
    ts.read(buffer, 5);
    buffer[5] = '\0';

    EXPECT_STREQ(buffer, "hello");
    EXPECT_EQ(ts.column(), 5);
}

TEST_F(TrackingStreamTest, ReadWithNewlines) {
    std::istringstream iss("ab\ncd\nef");
    TrackingStream ts(iss);

    char buffer[9];
    ts.read(buffer, 8);

    EXPECT_EQ(ts.line(), 2);
    EXPECT_EQ(ts.column(), 2);
}

TEST_F(TrackingStreamTest, GetlineTracking) {
    std::istringstream iss("first line\nsecond line");
    TrackingStream ts(iss);

    std::string line;
    std::getline(ts, line);

    EXPECT_EQ(line, "first line");
    EXPECT_EQ(ts.line(), 1);
    EXPECT_EQ(ts.column(), 0);
}

TEST_F(TrackingStreamTest, SeekAndTell) {
    std::istringstream iss("abcdef");
    TrackingStream ts(iss);

    ts.get();
    ts.get();

    auto pos = ts.tellg();
    EXPECT_EQ(pos, 2);

    ts.seekg(0);
    EXPECT_EQ(ts.line(), 0);
    EXPECT_EQ(ts.column(), 0);
}

TEST_F(TrackingStreamTest, SeekWithNewlines) {
    std::istringstream iss("abc\ndef\nghi");
    TrackingStream ts(iss);

    ts.seekg(5); // Position at 'e'
    EXPECT_EQ(ts.line(), 1);
    EXPECT_EQ(ts.column(), 1);
}

TEST_F(TrackingStreamTest, SeekRelative) {
    std::istringstream iss("abcdef");
    TrackingStream ts(iss);

    ts.get();
    ts.get(); // At position 2

    ts.seekg(2, std::ios::cur); // Move forward 2
    EXPECT_EQ(ts.column(), 4);
}

TEST_F(TrackingStreamTest, EOFState) {
    std::istringstream iss("ab");
    TrackingStream ts(iss);

    ts.get();
    ts.get();
    EXPECT_FALSE(ts.eof());

    ts.get(); // Try to read past end
    EXPECT_TRUE(ts.eof());
}

TEST_F(TrackingStreamTest, EmptyStream) {
    std::istringstream iss("");
    TrackingStream ts(iss);

    EXPECT_EQ(ts.get(), EOF);
    EXPECT_EQ(ts.line(), 0);
    EXPECT_EQ(ts.column(), 0);
}

TEST_F(TrackingStreamTest, IgnoreCharacters) {
    std::istringstream iss("abcdef");
    TrackingStream ts(iss);

    ts.ignore(3);
    EXPECT_EQ(ts.column(), 3);

    EXPECT_EQ(ts.get(), 'd');
}

TEST_F(TrackingStreamTest, IgnoreUntilDelimiter) {
    std::istringstream iss("hello\nworld");
    TrackingStream ts(iss);

    ts.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    EXPECT_EQ(ts.line(), 1);
    EXPECT_EQ(ts.column(), 0);

    EXPECT_EQ(ts.get(), 'w');
}

TEST_F(TrackingStreamTest, ExtractOperator) {
    std::istringstream iss("123 456");
    TrackingStream ts(iss);

    int a, b;
    ts >> a >> b;

    EXPECT_EQ(a, 123);
    EXPECT_EQ(b, 456);
    EXPECT_GT(ts.column(), 0);
}

TEST_F(TrackingStreamTest, MultipleNewlinesInRow) {
    std::istringstream iss("a\n\n\nb");
    TrackingStream ts(iss);

    ts.get(); // 'a'
    ts.get(); // '\n'
    EXPECT_EQ(ts.line(), 1);

    ts.get(); // '\n'
    EXPECT_EQ(ts.line(), 2);

    ts.get(); // '\n'
    EXPECT_EQ(ts.line(), 3);

    ts.get(); // 'b'
    EXPECT_EQ(ts.line(), 3);
    EXPECT_EQ(ts.column(), 1);
}

TEST_F(TrackingStreamTest, SeekToEnd) {
    std::istringstream iss("abcdef");
    TrackingStream ts(iss);

    ts.seekg(0, std::ios::end);
    EXPECT_EQ(ts.column(), 6);
}

TEST_F(TrackingStreamTest, ComplexMixedOperations) {
    std::istringstream iss("line1\nline2\nline3");
    TrackingStream ts(iss);

    std::string line;
    std::getline(ts, line);
    EXPECT_EQ(line, "line1");
    EXPECT_EQ(ts.line(), 1);

    ts.seekg(0);
    EXPECT_EQ(ts.line(), 0);
    EXPECT_EQ(ts.column(), 0);

    char c;
    ts.get(c);
    EXPECT_EQ(c, 'l');
    EXPECT_EQ(ts.column(), 1);

    ts.unget();
    EXPECT_EQ(ts.column(), 0);
}

TEST_F(TrackingStreamTest, GoodBadFailStates) {
    std::istringstream iss("test");
    TrackingStream ts(iss);

    EXPECT_TRUE(ts.good());
    EXPECT_FALSE(ts.bad());
    EXPECT_FALSE(ts.fail());

    ts.get();
    ts.get();
    ts.get();
    ts.get();
    EXPECT_TRUE(ts.good());

    ts.get(); // Try to read past end
    EXPECT_TRUE(ts.eof());
}
