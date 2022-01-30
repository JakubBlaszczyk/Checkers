package com.pk.logic;

import com.pk.logic.exceptions.InvalidDirection;

public enum Direction {
  UP(1), DOWN(-1), LEFT(-1), RIGHT(1);

  private Direction(Integer direction) {
    this.value = direction;
  }

  public Integer getDirection() {
    return this.value;
  }

  public Direction getOppositeDirection() {
    switch (this) {
      case UP:
        return DOWN;
      case DOWN:
        return UP;
      case LEFT:
        return RIGHT;
      case RIGHT:
        return LEFT;
      default:
        throw new InvalidDirection();
    }
  }

  static public Direction toDirection(Integer value) {
    return value < 0 ? DOWN : UP;
  }

  Integer value;
}
