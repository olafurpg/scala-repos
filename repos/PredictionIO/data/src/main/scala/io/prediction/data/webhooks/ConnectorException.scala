/** Copyright 2015 TappingStone, Inc.
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *     http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */
package io.prediction.data.webhooks

/** Webhooks Connnector Exception
  *
  * @param message the detail message
  * @param cause the cause
  */
private[prediction] class ConnectorException(message: String, cause: Throwable)
    extends Exception(message, cause)

  /** Webhooks Connnector Exception with cause being set to null
    *
    * @param message the detail message
    */
  def this(message: String) = this(message, null)
