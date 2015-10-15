package cn.edu.neu.chiewen.cknn.settings

import org.springframework.context.support.FileSystemXmlApplicationContext

object Settings {
  val applicationContext = new FileSystemXmlApplicationContext("CkNN/beans.xml");
  val globalSetting = applicationContext.getBean("globalSetting").asInstanceOf[GlobalSetting]
  //BEAN_PATH)
  private val BEAN_PATH = getClass.getResource("/beans.xml").getPath
}