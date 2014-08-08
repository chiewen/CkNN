package cn.edu.neu.chiewen.cknn

import cn.edu.neu.chiewen.cknn.settings.Settings
import cn.edu.neu.chiewen.cknn.site.SiteGenerator
import cn.edu.neu.chiewen.cknn.vtree.{VTree, VTreeIO}

object DataGenerator extends App {
    VTreeIO.write(VTree(SiteGenerator.getSites()),
    Settings.globalSetting.dir_write + "data.vtd",
    Settings.globalSetting.dir_write + "index.vti",
    Settings.globalSetting.dir_write + "tree.vtm")
}