package com.pk;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.TimeUnit;

import org.apache.commons.collections4.BidiMap;
import org.apache.commons.collections4.bidimap.DualHashBidiMap;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class App {
  public static void main(String[] args) throws InterruptedException {
    Properties prop = new Properties();
    try (InputStream fis = App.class.getResourceAsStream("/server.config")) {
      prop.load(fis);
    } catch (IOException ex) {
      log.error("Config is dead: " + ex.getMessage());
      return;
    }
    if (!ConfigValidator.validateConfig(prop)) {
      log.error("Config is not valid");
      return;
    }
    String ip = prop.getProperty("server.ipBind");
    Integer port = Integer.parseInt(prop.getProperty("server.port"));
    try {
      Server server = new Server(ip, port, new HashMap<>(), new DualHashBidiMap<>(), new HashSet<>());
      server.call();
    } catch (IOException e) {
      log.error("Server is dead");
      e.printStackTrace();
    } catch (Exception e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    TimeUnit.SECONDS.sleep(1000000);
  }
}
