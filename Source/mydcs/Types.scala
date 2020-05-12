package mydcs

import Value._

object Types{
  type Tag = String
  type Column = String
  type Data = (List[Column], Set[List[PrimitiveValue]])
}
