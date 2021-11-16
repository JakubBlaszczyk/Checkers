package com.pk;

/** Main class wrapper used to bypass problems with creating uber-jar */
public class SuperMain {
  /**
   * Calls App main()
   *
   * @param args Arguments passed by command line
   */
  public static void main(String[] args) {
    App.main(args);
  }
}