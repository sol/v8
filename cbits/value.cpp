#include <v8.h>
#include <string.h>

using namespace v8;

static_assert(sizeof(Handle<Value>) == sizeof(void*),
  "We treat Handle<Value> "
  "as a pointer, but your compiler thinks it has a different size.  You can "
  "request support for your platform by opening a ticket at "
  "https://github.com/sol/v8/issues."
  );

static_assert(sizeof(Local<String>) == sizeof(void*),
  "We treat Local<String>"
  "as a pointer, but your compiler thinks it has a different size.  You can "
  "request support for your platform by opening a ticket at "
  "https://github.com/sol/v8/issues."
  );

extern "C" {
Handle<Value> mkUndefined() {
  return Undefined();
}

Local<Value> c_mkString(const char* str, int n) {
  return String::New(str, n);
}

Local<String> valueToString(Handle<Value> value) {
  return value->ToString();
}

int stringUtf8Length(Handle<String> str) {
  return str->Utf8Length();
}

void stringUtf8Value(Handle<String> str, char* dst) {
  String::Utf8Value src(str);
  memcpy(dst, *src, src.length());
}

typedef void (*ActionCallback)(void);
void c_withHandleScope(ActionCallback action) {
  HandleScope scope;
  action();
}
}
