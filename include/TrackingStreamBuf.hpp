#pragma once

#include <ios>
#include <iosfwd>
#include <streambuf>

namespace flynt {

	class TrackingStreamBuf : public std::streambuf {
		std::streambuf *_source;
		std::streamoff _line, _col, _last_pos;
		char _last_char;
	public:
		TrackingStreamBuf(std::streambuf *source);

		std::streampos line() const { return _line; }
		std::streampos column() const { return _col; }
	protected:
		int_type underflow() override;
		int_type uflow() override;
		int_type pbackfail(int_type c) override;
		std::streampos seekoff(std::streamoff off, std::ios_base::seekdir way, std::ios_base::openmode which) override;
		std::streampos seekpos(std::streampos pos, std::ios_base::openmode which) override;
		std::streamsize xsgetn(char* s, std::streamsize n) override;
	private:
		void update_position(char c, bool backward);
		void update_position_backward(char c);
		void recalculate_column();
		void recalculate_position();
	};

}
