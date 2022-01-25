package com.pk.logic;

import com.pk.frontend.checkers.MoveType;

public interface Logic {

  public MoveType update(Integer newX, Integer newY, Integer oldX, Integer oldY);

}
