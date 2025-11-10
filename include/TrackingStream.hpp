#pragma once

#include "TrackingStreamBuf.hpp"
#include <ios>
#include <istream>

namespace flynt {

	class TrackingStream : public std::istream {
		TrackingStreamBuf _buf;
	public:
		TrackingStream(std::istream &source);

		std::streamoff line() const { return _buf.line(); }
		std::streamoff column() const { return _buf.column(); }
	};

}
