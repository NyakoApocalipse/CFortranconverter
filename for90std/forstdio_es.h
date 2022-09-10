/* TODO: when write or read has more than one placeholders, the position in source string cannot move as in a file stream can */
// read
#include <utility>
inline void _forread_noargs(const char * f, IOFormat & format) {
	format.strip_front();
}

template <typename T>
void _str_sscanf(const char * f, const std::string & _format, T & x) {
	sscanf(f, _format.c_str(), &x);
}
inline void _str_sscanf(const char * f, const std::string & _format, std::string & x) {
	// std::ifstream ifs(f);
	// ifs >> x;
	char buf[1024];
	sscanf(f, _format.c_str(), buf);
	x = buf;
}

// read formatted step 2
template <typename T>
void _forread_one(const char * f, IOFormat & format, T & x) {
	// strip front
	_forread_noargs(f, format);
	sscanf(f, format.next_editing().c_str(), &x);
};
inline void _forread_one(const char * f, IOFormat & format, std::string & x) {
	// strip front
	_forread_noargs(f, format);
	std::string fmt = format.next_editing();
	_str_sscanf(f, fmt, x);
};
template <typename T>
void _forread_one_arrf(const char * f, IOFormat & format, farray<T> & x) {
	// clear front
	_forread_noargs(f, format);
	auto iter = x.cbegin();
	for (fsize_t i = 0; i < x.flatsize(); i++)
	{
		_forread_dispatch(f, format, *(iter + i));
	}
};
// read formatted step 1
template <typename T>
void _forread_dispatch(const char * f, IOFormat & format, farray<T> * x) {
	_forread_one_arrf(f, format, *x);
};
template <typename T>
void _forread_dispatch(const char * f, IOFormat & format, T * x) {
	_forread_one(f, format, *x);
};
inline void _forread_one(const char * f, IOFormat & format, std::string * x) {
	_forread_one(f, format, *x);
};
template <typename ... Types>
void _forread_dispatch(const char * f, IOFormat & format, IOStuff<Types...> & iostuff) {
	foreach_tuple(iostuff.tp, [&](auto & x) {
		_forread_dispatch(f, x);
	});
};
template <typename T, typename F>
void _forread_dispatch(const char * f, IOFormat & format, ImpliedDo<T, F> & l) {
	format.strip_front();
	while (l.has_next())
	{
		_forread_dispatch(f, format, l.get_next());
	}
};

// read formatted step 0

inline void forread(const char *f, IOFormat &format){
    // iteration terminal
};

template <typename T, typename... Args>
void forread(const std::string f, IOFormat & format, T&& x, Args&&... args) {
  _forread_dispatch(f.c_str(), format, x);
  forread(f.c_str(), format, std::forward<Args>(args)...);
};

template <typename T, typename... Args>
void forread(const std::string f, const std::string & format, T && x, Args&&... args) {
	IOFormat _format(format);
	_forread_dispatch(f.c_str(), _format, x);
	forread(f.c_str(), _format, std::forward<Args>(args)...);
};

template <typename T, typename... Args>
void forread(const char * f, IOFormat & format, T&& x, Args&&... args) {
	_forread_dispatch(f, format, x);
	forread(f, format, std::forward<Args>(args)...);
};

template <typename T, typename... Args>
void forread(const char * f, const std::string & format, T && x, Args&&... args) {
	IOFormat _format(format);
	_forread_dispatch(f, _format, x);
	forread(f, _format, std::forward<Args>(args)...);
};

// free format
// read free step 2

inline void _forreadfree_one(const char * f, int & x) {
	int res = sscanf(f, "%d", &x);

};
inline void _forreadfree_one(const char * f, long long & x) {
	sscanf(f, "%lld", &x);
};
inline void _forreadfree_one(const char * f, double & x) {
	sscanf(f, "%lf", &x);
};
inline void _forreadfree_one(const char * f, long double & x) {
	sscanf(f, "%Lf", &x);
};
inline void _forreadfree_one(const char * f, std::string & x) {
	_str_sscanf(f, "%s", x);
};
inline void _forreadfree_one(const char * f, bool & x) {
	char bool_str[10];
	sscanf(f, "%s", bool_str);
	if (bool_str[0] == 'T' || bool_str[0] == 't')
	{
		x = true;
	}
	else {
		x = false;
	}
};
inline void _forreadfree_one(const char * f, char * x) {
	sscanf(f, "%s", x);
};
inline void _forreadfree_one(const char * f, char & x) {
	sscanf(f, "%c", &x);
};


template <typename T>
void _forreadfree_one_arrf(const char * f, farray<T> & x) {
	auto iter = x.begin();
	for (fsize_t i = 0; i < x.flatsize(); i++)
	{
		_forreadfree_one(f, *(iter + i));
	}
};
// read free step 1

template <typename T>
void _forreadfree_dispatch(const char * f, farray<T> * x) {
	_forreadfree_one_arrf(f, *x);
};
template <typename T>
void _forreadfree_dispatch(const char * f, T * x) {
	_forreadfree_one(f, *x);
};
template <typename ... Types>
void _forreadfree_dispatch(const char * f, IOStuff<Types...> & iostuff) {
	foreach_tuple(iostuff.tp, [&](auto & x) {
		_forreadfree_dispatch(f, x);
	});
};
template <typename T, typename F>
void _forreadfree_dispatch(const char * f, ImpliedDo<T, F> & l) {
	while (l.has_next())
	{
		_forreadfree_dispatch(f, l.get_next());
	}
};
template <typename ... Types>
void _forreadfree_dispatch(const char * f, IOStuff<Types...> && iostuff) {
	foreach_tuple(iostuff.tp, [&](auto & x) {
		_forreadfree_dispatch(f, x);
	});
};
template <typename T, typename F>
void _forreadfree_dispatch(const char * f, ImpliedDo<T, F> && l) {
	while (l.has_next())
	{
		_forreadfree_dispatch(f, l.get_next());
	}
};
// read free step 0
template <typename T>
void forreadfree(const std::string f, T&& x) {
  _forreadfree_dispatch(f.c_str(), x);
};

template <typename T, typename... Args>
void forreadfree(const std::string f, T&& x, Args &&... args) {
  _forreadfree_dispatch(f.c_str(), x);
  forreadfree(f.c_str(), std::forward<Args>(args)...);
};

template <typename T>
void forreadfree(const char * f, T&& x) {
	_forreadfree_dispatch(f, x);
};

template <typename T, typename... Args>
void forreadfree(const char * f, T&& x, Args &&... args) {
	_forreadfree_dispatch(f, x);
	forreadfree(f, std::forward<Args>(args)...);
};
