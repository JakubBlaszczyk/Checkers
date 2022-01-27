package com.pk.logic;

public enum LogicTile {
  WHITE(-1, 0), BLACK(1, 0), WHITE_PAWN(-1, -1), WHITE_KING(-1, 1), BLACK_PAWN(1, -1), BLACK_KING(1, 1), EMPTY(0, 0);

  private LogicTile(Integer color, Integer rank) {
    this.color = color;
    this.rank = rank;
  }

  public Boolean isEmpty() {
    return this.color == 0 && this.rank == 0;
  }

  public Boolean isKing() {
    return this.rank == 1;
  }

  public Boolean isPawn() {
    return this.rank == -1;
  }

  public Boolean isWhite() {
    return this.color == -1;
  }

  public Boolean isBlack() {
    return this.color == 1;
  }

  public Boolean isOppositeColor(LogicTile tile) {
    return (this.color * -1) == tile.getColor();
  }

  public Boolean compareColors(LogicTile tile) {
    return this.color.equals(tile.getColor());
  }

  public Integer getColor() {
    return this.color;
  }

  public Integer getOppositeColor() {
    return this.color * -1;
  }

  public String toSymbol() {
    switch (this) {
      case WHITE_KING:
        return "K";
      case BLACK_KING:
        return "R";
      case WHITE_PAWN:
        return "W";
      case BLACK_PAWN:
        return "B";
      case EMPTY:
        return " ";
      default:
        return "X";
    }
  }

  public LogicTile promote() {
    switch (this) {
      case WHITE_PAWN:
        return WHITE_KING;
      case BLACK_PAWN:
        return BLACK_KING;
      default:
        return EMPTY;
    }
  }

  public LogicTile getOppositeTile() {
    switch (this) {
      case WHITE:
      case WHITE_PAWN:
      case WHITE_KING:
        return BLACK;
      case BLACK:
      case BLACK_PAWN:
      case BLACK_KING:
        return WHITE;
      default:
        return EMPTY;
    }
  }

  private Integer color;
  private Integer rank;
}