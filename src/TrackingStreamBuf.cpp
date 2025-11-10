#include "TrackingStreamBuf.hpp"
#include <iosfwd>

using flynt::TrackingStreamBuf;

TrackingStreamBuf::TrackingStreamBuf(std::streambuf *source)
	: _source(source), _line(0), _col(0), _last_pos(0), _last_char('\0') {}

TrackingStreamBuf::int_type TrackingStreamBuf::underflow() {
	return _source->sgetc();
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
		recalculate_position();
	return result;
}

std::streampos TrackingStreamBuf::seekpos(std::streampos pos, std::ios_base::openmode which) {
	std::streampos result = _source->pubseekpos(pos, which);
	if (result != std::streampos(-1))
		recalculate_position();
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

void TrackingStreamBuf::update_position_backward(char c) {
	if (_last_char == '\n') {
		--_line;
		recalculate_column();
	} else if (_col > 0) {
		--_col;
	}
	_last_char = c;
}

void TrackingStreamBuf::recalculate_column() {
	std::streampos cur = _source->pubseekoff(0, std::ios::cur);
	_col = 0;
	std::streamoff pos = cur;
	while (pos > 0) {
		_source->pubseekoff(-1, std::ios::cur);
		--pos;
		int_type c = _source->sgetc();
		if (c == '\n' || c == EOF)
			break;
	}
	_source->pubseekpos(pos);
}

void TrackingStreamBuf::recalculate_position() {
	std::streamoff cur = _source->pubseekoff(0, std::ios_base::cur);
	_source->pubseekpos(0);
	_line = 0;
	_col = 0;

	std::streamoff pos = 0;
	while (pos < cur) {
		int_type c = _source->sbumpc();
		if (c == EOF) break;

		if (c == '\n') {
			++_line;
			_col = 0;
		} else {
			++_col;
		}
		++pos;
	}
}
