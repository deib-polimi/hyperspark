package it.polimi.hyperh.apps

/**
 * @author Nemanja
 */
class MainApp {
  def run() {
    //new TesterApp().run()
    //new YarnClusterApp().run()
    new CooperativeSame().run()
  }
}
//companion object which provides an entrypoint to the environment 
//(when it is not specified, e.g. by using spark-submit --class)
object MainApp {
  def main(args: Array[String]) {
     new MainApp().run()
  }
}