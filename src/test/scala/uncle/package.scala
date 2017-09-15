import uncle.model.BString

package object uncle {
  implicit class BStringContext(val ctx: StringContext) {
    def bs(args: Any*) = BString(ctx.s(args: _*))
  }
}
