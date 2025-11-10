#include "TrackingStreamBuf.hpp"
#include <iosfwd>
#include <numbers>

using flynt::TrackingStreamBuf;

TrackingStreamBuf::TrackingStreamBuf(std::streambuf *source)
	: _source(source), _line(0), _col(0), _last_pos(0), _last_char('\0') {}

TrackingStreamBuf::int_type TrackingStreamBuf::underflow() {
	int_type c = _source->sgetc();
	if (c == EOF) return EOF;

	_buffer = static_cast<char>(c);
	setg(&_buffer, &_buffer, &_buffer + 1);
	return _buffer;
}

TrackingStreamBuf::int_type TrackingStreamBuf::uflow() {
	int_type c = _source->sbumpc();
	if (c != EOF)
		update_position(static_cast<char>(c), false);
	return c;
}

TrackingStreamBuf::int_type TrackingStreamBuf::pbackfail(int_type c) {
	int_type result;
	if (c == EOF)
		result = _source->sungetc();
	else
		result = _source->sputbackc(static_cast<char>(c));

	if (result != EOF)
		update_position_backward(static_cast<char>(result));
	return result;
}

std::streampos TrackingStreamBuf::seekoff(std::streamoff off, std::ios_base::seekdir way, std::ios_base::openmode which) {
	std::streampos result = _source->pubseekoff(off, way, which);
	if (result != std::streampos(-1))
		recalculate_position_non_destructive();
	return result;
}

std::streampos TrackingStreamBuf::seekpos(std::streampos pos, std::ios_base::openmode which) {
	std::streampos result = _source->pubseekpos(pos, which);
	if (result != std::streampos(-1))
		recalculate_position_non_destructive();
	return result;
}

std::streamsize TrackingStreamBuf::xsgetn(char* s, std::streamsize n) {
	std::streamsize count = _source->sgetn(s, n);
	for (std::streamsize i = 0; i < count; ++i)
		update_position(s[i], false);
	return count;
}

void TrackingStreamBuf::update_position(char c, bool backward) {
	if (!backward) {
		_last_char = c;
		if (c == '\n') {
			++_line;
			_col = 0;
		} else {
			++_col;
		}
	}
}

void TrackingStreamBuf::update_position_backward() {
	std::streampos cur = _source->pubseekoff(0, std::ios_base::cur);
	if (cur == std::streampos(-1)) return;

	std::streampos newpos = _source->pubseekoff(-1, std::ios_base::cur);
	if (newpos == std::streampos(-1)) return;

	recalculate_position_at(newpos);
}

void TrackingStreamBuf::recalculate_column() {
	std::streampos cur = _source->pubseekoff(0, std::ios::cur);
	_col = 0;
	std::streampos pos = cur;
	while (pos > 0) {
		_source->pubseekoff(-1, std::ios::cur);
		pos -= 1;
		int_type c = _source->sgetc();
		if (c == '\n' || c == EOF)
			break;
	}
	_source->pubseekpos(pos);
}

void TrackingStreamBuf::recalculate_position_at(std::streampos end) {
	std::streampos cur = _source->pubseekoff(0, std::ios_base::cur);
	_source->pubseekpos(0);
	_line = 0;
	_col = 0;

	for (std::streamoff i = 0; i < end; ++i) {
		int_type c = _source->sbumpc();
		if (c == EOF) break;
		if (c == '\n') {
			++_line;
			_col = 0;
		} else {
			++_col;
		}
	}
	_source->pubseekpos(cur);
}

void TrackingStreamBuf::recalculate_position_non_destructive() {
	std::streampos cur = _source->pubseekoff(0, std::ios::cur);
	if (cur == std::streampos(-1)) return;

	_source->pubseekpos(0);
	_line = 0;
	_col = 0;

	for (std::streamoff i = 0; i < end; ++i) {
		int_type c = _source->sbumpc();
		if (c == EOF) break;
		if (c == '\n') {
			++_line;
			_col = 0;
		} else {
			++_col;
		}
		_source->pubseekoff(1, std::ios::cur);
	}
	_source->pubseekpos(cur);
}
