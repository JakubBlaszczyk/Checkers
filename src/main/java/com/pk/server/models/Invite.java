package com.pk.server.models;

import java.net.Socket;

import lombok.Value;

@Value
public class Invite {
  String nick;
  String profileImg;
  Socket sock;
}
