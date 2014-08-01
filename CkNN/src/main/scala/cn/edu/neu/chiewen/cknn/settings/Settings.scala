package cn.edu.neu.chiewen.cknn.settings

import org.springframework.context.support.FileSystemXmlApplicationContext

object Settings {
  private val BEAN_PATH = "/" + getClass.getResource("/beans.xml").getPath

  val applicationContext = new FileSystemXmlApplicationContext(BEAN_PATH)
  val globalSetting = applicationContext.getBean("globalSetting").asInstanceOf[GlobalSetting]
}