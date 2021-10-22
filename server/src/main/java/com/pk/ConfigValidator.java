package com.pk;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ConfigValidator {
  private ConfigValidator(){}

  static boolean validateConfig(Properties prop) {
    List<String> props = List.of("server.ipBind", "server.port", "server.logsFolder");
    for (String str : props) {
      if (prop.getProperty(str) == null) {
        log.error("Missing " + str);
        return false;
      }
    }
      return true;
  }
}
