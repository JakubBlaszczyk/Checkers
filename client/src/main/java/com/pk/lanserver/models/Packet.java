package com.pk.lanserver.models;

import java.net.InetAddress;
import lombok.Value;

/** Wrapper class containing information about move. */
@Value
public class Packet {
  InetAddress ip;
  Integer port;
  String msg;
}
