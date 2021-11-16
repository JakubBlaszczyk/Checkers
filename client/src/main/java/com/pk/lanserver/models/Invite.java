package com.pk.lanserver.models;

import java.net.Socket;
import lombok.Value;

/** Wrapper class containing information about invitation. */
@Value
public class Invite {
  String nick;
  String profileImg;
  Socket sock;
  String code;
}
