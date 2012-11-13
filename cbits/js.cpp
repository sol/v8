#include <v8.h>

using namespace v8;

extern "C" {

Local<Value> c_runScript(const char* input) {
  HandleScope scope;
  Local<Value> value = Script::Compile(String::New(input))->Run();
  return scope.Close(value);
}

}
