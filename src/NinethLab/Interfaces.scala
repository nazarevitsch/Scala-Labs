package NinethLab

import scalashop.{Img, RGBA}

trait HorizontalBoxBlurInterface {
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit
}

trait VerticalBoxBlurInterface {
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit
}

trait BoxBlurKernelInterface {
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA
}
