#include <gtest/gtest.h>
#include "TrackingStreamBuf.hpp"
#include <sstream>
#include <string>

using namespace flynt;

class TrackingStreamBufTest : public ::testing::Test {
protected:
    void SetUp() override {}
    void TearDown() override {}
};

TEST_F(TrackingStreamBufTest, InitialPosition) {
    std::istringstream iss("test");
    TrackingStreamBuf buf(iss.rdbuf());

    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 0);
}

TEST_F(TrackingStreamBufTest, SingleCharacterRead) {
    std::istringstream iss("a");
    TrackingStreamBuf buf(iss.rdbuf());

    EXPECT_EQ(buf.sbumpc(), 'a');
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 1);
}

TEST_F(TrackingStreamBufTest, MultipleCharacterRead) {
    std::istringstream iss("abc");
    TrackingStreamBuf buf(iss.rdbuf());

    EXPECT_EQ(buf.sbumpc(), 'a');
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 1);

    EXPECT_EQ(buf.sbumpc(), 'b');
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 2);

    EXPECT_EQ(buf.sbumpc(), 'c');
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 3);
}

TEST_F(TrackingStreamBufTest, NewlineIncreasesLine) {
    std::istringstream iss("a\nb");
    TrackingStreamBuf buf(iss.rdbuf());

    EXPECT_EQ(buf.sbumpc(), 'a');
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 1);

    EXPECT_EQ(buf.sbumpc(), '\n');
    EXPECT_EQ(buf.line(), 1);
    EXPECT_EQ(buf.column(), 0);

    EXPECT_EQ(buf.sbumpc(), 'b');
    EXPECT_EQ(buf.line(), 1);
    EXPECT_EQ(buf.column(), 1);
}

TEST_F(TrackingStreamBufTest, MultipleNewlines) {
    std::istringstream iss("a\n\n\nb");
    TrackingStreamBuf buf(iss.rdbuf());

    buf.sbumpc(); // 'a'
    EXPECT_EQ(buf.line(), 0);

    buf.sbumpc(); // '\n'
    EXPECT_EQ(buf.line(), 1);
    EXPECT_EQ(buf.column(), 0);

    buf.sbumpc(); // '\n'
    EXPECT_EQ(buf.line(), 2);
    EXPECT_EQ(buf.column(), 0);

    buf.sbumpc(); // '\n'
    EXPECT_EQ(buf.line(), 3);
    EXPECT_EQ(buf.column(), 0);

    buf.sbumpc(); // 'b'
    EXPECT_EQ(buf.line(), 3);
    EXPECT_EQ(buf.column(), 1);
}

TEST_F(TrackingStreamBufTest, UnderflowDoesNotAdvance) {
    std::istringstream iss("ab");
    TrackingStreamBuf buf(iss.rdbuf());

    EXPECT_EQ(buf.sgetc(), 'a');
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 0);

    EXPECT_EQ(buf.sgetc(), 'a');
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 0);
}

TEST_F(TrackingStreamBufTest, UflowAdvances) {
    std::istringstream iss("ab");
    TrackingStreamBuf buf(iss.rdbuf());

    EXPECT_EQ(buf.sbumpc(), 'a');
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 1);

    EXPECT_EQ(buf.sbumpc(), 'b');
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 2);
}

TEST_F(TrackingStreamBufTest, PutbackSingleCharacter) {
    std::istringstream iss("abc");
    TrackingStreamBuf buf(iss.rdbuf());

    buf.sbumpc(); // 'a'
    buf.sbumpc(); // 'b'
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 2);

    EXPECT_NE(buf.sputbackc('b'), EOF);
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 1);
}

TEST_F(TrackingStreamBufTest, PutbackNewline) {
    std::istringstream iss("a\nb");
    TrackingStreamBuf buf(iss.rdbuf());

    buf.sbumpc(); // 'a'
    buf.sbumpc(); // '\n'
    EXPECT_EQ(buf.line(), 1);
    EXPECT_EQ(buf.column(), 0);

    EXPECT_NE(buf.sputbackc('\n'), EOF);
    EXPECT_EQ(buf.line(), 0);
    // Column should be recalculated (this tests the recalculate_column function)
}

TEST_F(TrackingStreamBufTest, SungetcBehavior) {
    std::istringstream iss("abc");
    TrackingStreamBuf buf(iss.rdbuf());

    buf.sbumpc(); // 'a'
    buf.sbumpc(); // 'b'
    EXPECT_EQ(buf.column(), 2);

    EXPECT_NE(buf.sungetc(), EOF);
    EXPECT_EQ(buf.column(), 1);
}

TEST_F(TrackingStreamBufTest, XsgetnMultipleCharacters) {
    std::istringstream iss("abcdef");
    TrackingStreamBuf buf(iss.rdbuf());

    char buffer[4];
    EXPECT_EQ(buf.sgetn(buffer, 3), 3);
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 3);
}

TEST_F(TrackingStreamBufTest, XsgetnWithNewlines) {
    std::istringstream iss("ab\ncd\nef");
    TrackingStreamBuf buf(iss.rdbuf());

    char buffer[10];
    EXPECT_EQ(buf.sgetn(buffer, 8), 8);
    EXPECT_EQ(buf.line(), 2);
    EXPECT_EQ(buf.column(), 2);
}

TEST_F(TrackingStreamBufTest, SeekToBeginning) {
    std::istringstream iss("abc\ndef");
    TrackingStreamBuf buf(iss.rdbuf());

    buf.sbumpc(); // 'a'
    buf.sbumpc(); // 'b'
    buf.sbumpc(); // 'c'
    buf.sbumpc(); // '\n'
    buf.sbumpc(); // 'd'

    EXPECT_EQ(buf.line(), 1);
    EXPECT_EQ(buf.column(), 1);

    buf.pubseekpos(0);
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 0);
}

TEST_F(TrackingStreamBufTest, SeekToMiddle) {
    std::istringstream iss("abc\ndef\nghi");
    TrackingStreamBuf buf(iss.rdbuf());

    buf.pubseekpos(5); // Position at 'e'
    // After recalculation, should be at line 1, column 1
    EXPECT_EQ(buf.line(), 1);
    EXPECT_EQ(buf.column(), 1);
}

TEST_F(TrackingStreamBufTest, SeekOffsetFromCurrent) {
    std::istringstream iss("abcdef");
    TrackingStreamBuf buf(iss.rdbuf());

    buf.sbumpc(); // 'a'
    buf.sbumpc(); // 'b'

    buf.pubseekoff(2, std::ios::cur);
    // Should recalculate to position 4
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 4);
}

TEST_F(TrackingStreamBufTest, SeekOffsetFromBeginning) {
    std::istringstream iss("abc\ndef");
    TrackingStreamBuf buf(iss.rdbuf());

    buf.pubseekoff(5, std::ios::beg);
    EXPECT_EQ(buf.line(), 1);
    EXPECT_EQ(buf.column(), 1);
}

TEST_F(TrackingStreamBufTest, SeekOffsetFromEnd) {
    std::istringstream iss("abcdef");
    TrackingStreamBuf buf(iss.rdbuf());

    buf.pubseekoff(-2, std::ios::end);
    // Should be at position 4
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 4);
}

TEST_F(TrackingStreamBufTest, EOFHandling) {
    std::istringstream iss("a");
    TrackingStreamBuf buf(iss.rdbuf());

    EXPECT_EQ(buf.sbumpc(), 'a');
    EXPECT_EQ(buf.sbumpc(), EOF);
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 1);
}

TEST_F(TrackingStreamBufTest, EmptyStream) {
    std::istringstream iss("");
    TrackingStreamBuf buf(iss.rdbuf());

    EXPECT_EQ(buf.sbumpc(), EOF);
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 0);
}

TEST_F(TrackingStreamBufTest, ComplexSeekAndRead) {
    std::istringstream iss("line1\nline2\nline3");
    TrackingStreamBuf buf(iss.rdbuf());

    // Read first line
    char buffer[6];
    buf.sgetn(buffer, 6); // "line1\n"
    EXPECT_EQ(buf.line(), 1);
    EXPECT_EQ(buf.column(), 0);

    // Seek back to beginning
    buf.pubseekpos(0);
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 0);

    // Read again
    EXPECT_EQ(buf.sbumpc(), 'l');
    EXPECT_EQ(buf.line(), 0);
    EXPECT_EQ(buf.column(), 1);
}

TEST_F(TrackingStreamBufTest, OnlyNewlines) {
    std::istringstream iss("\n\n\n");
    TrackingStreamBuf buf(iss.rdbuf());

    buf.sbumpc();
    EXPECT_EQ(buf.line(), 1);
    EXPECT_EQ(buf.column(), 0);

    buf.sbumpc();
    EXPECT_EQ(buf.line(), 2);
    EXPECT_EQ(buf.column(), 0);

    buf.sbumpc();
    EXPECT_EQ(buf.line(), 3);
    EXPECT_EQ(buf.column(), 0);
}

TEST_F(TrackingStreamBufTest, CarriageReturnHandling) {
    std::istringstream iss("a\rb\rc");
    TrackingStreamBuf buf(iss.rdbuf());

    buf.sbumpc(); // 'a'
    EXPECT_EQ(buf.column(), 1);

    buf.sbumpc(); // '\r'
    EXPECT_EQ(buf.column(), 2);
    EXPECT_EQ(buf.line(), 0);

    buf.sbumpc(); // 'b'
    EXPECT_EQ(buf.column(), 3);
}
