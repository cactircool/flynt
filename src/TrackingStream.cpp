#include "TrackingStream.hpp"
#include <istream>

using flynt::TrackingStream;

TrackingStream::TrackingStream(std::istream &source)
	: std::istream(&_buf), _buf(source.rdbuf()) {}
