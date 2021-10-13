package com.pk.server.models;

import java.net.InetAddress;

import lombok.Value;

@Value
public class Packet {
  InetAddress ip;
  Integer port;
  String msg;
}
