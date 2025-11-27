package hiroki1117

import munit.CatsEffectSuite

class HelloWorldSuite extends CatsEffectSuite {

  test("test hello world says hi") {
    HelloWorld.say().map(it => assertEquals(it, "Hello Cats!"))
  }
}
