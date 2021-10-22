package com.pk.logic;

import java.util.List;

import java.util.ArrayList;

import com.pk.logic.exceptions.MandatoryKillMove;
import com.pk.logic.exceptions.MoreThanOneMoveMade;
import com.pk.logic.exceptions.MoveOnAlreadyTakenSpace;
import com.pk.logic.exceptions.VerticalOrHorizontalMove;

public class BasicLogic implements Logic {
  public BasicLogic(List<List<Piece>> board) {
    // fill the board
    this.board = new ArrayList<>(board.size());
    for (int i = 0; i < board.size(); ++i) {
      this.board.add(new ArrayList<>(board.get(i)));
    }
    // analyze white and black positions
    this.white = findAllWhite(board);
    this.black = findAllBlack(board);
  }

  public void update(List<List<Piece>> board)
      throws MandatoryKillMove, VerticalOrHorizontalMove, MoreThanOneMoveMade, MoveOnAlreadyTakenSpace {
    ArrayList<PiecePosition> white = findAllWhite(board);
    ArrayList<PiecePosition> black = findAllBlack(board);
    ArrayList<Integer> positions;

    positions = findOneProperMove(board);

    if (Boolean.FALSE.equals(isKillMove(board))) {
      if (Boolean.TRUE.equals(wasKillMoveAvaliable(white, black))) {
        throw new MandatoryKillMove();
      } else {
        // proceed with program and board update
      }
    }
  }

  private ArrayList<Integer> findOneProperMove(List<List<Piece>> board)
      throws MoreThanOneMoveMade, MoveOnAlreadyTakenSpace, VerticalOrHorizontalMove  {
    ArrayList<PiecePosition> white = findAllWhite(board);
    ArrayList<PiecePosition> black = findAllBlack(board);
    isNonDiagonalMove(white, black);
    return new ArrayList<>();
  }

  private Boolean isKillMove(List<List<Piece>> board) {

    return false;
  }

  private Boolean wasKillMoveAvaliable(List<PiecePosition> white, List<PiecePosition> black) {
    return false;
  }

  public String toString() {
    StringBuilder result = new StringBuilder();
    for (int i = 0; i < this.board.size(); ++i) {
      for (int j = 0; j < this.board.get(i).size(); ++j) {
        switch (this.board.get(i).get(j)) {
        case EMPTY:
          result.append("0");
          break;
        case WHITE_KING:
          result.append("I");
          break;
        case WHITE_PAWN:
          result.append("W");
          break;
        case BLACK_PAWN:
          result.append("B");
          break;
        case BLACK_KING:
          result.append("X");
          break;
        }
      }
      result.append("\n");
    }
    return result.toString();
  }

  private ArrayList<PiecePosition> findAllWhite(List<List<Piece>> board) {
    ArrayList<PiecePosition> whiteLocal = new ArrayList<>();
    for (int i = 0; i < board.size(); ++i) {
      for (int j = 0; j < board.get(i).size(); ++j) {
        if (board.get(i).get(j).equals(Piece.WHITE_KING) || board.get(i).get(j).equals(Piece.WHITE_PAWN)) {
          whiteLocal.add(new PiecePosition(Integer.valueOf(i), Integer.valueOf(j), board.get(i).get(j)));
        }
      }
    }
    return whiteLocal;
  }

  private ArrayList<PiecePosition> findAllBlack(List<List<Piece>> board) {
    ArrayList<PiecePosition> blackLocal = new ArrayList<>();
    for (int i = 0; i < board.size(); ++i) {
      for (int j = 0; j < board.get(i).size(); ++j) {
        if (board.get(i).get(j).equals(Piece.BLACK_KING) || board.get(i).get(j).equals(Piece.BLACK_PAWN)) {
          blackLocal.add(new PiecePosition(Integer.valueOf(i), Integer.valueOf(j), board.get(i).get(j)));
        }
      }
    }
    return blackLocal;
  }

  private void isNonDiagonalMove(List<PiecePosition> white, List<PiecePosition> black) throws VerticalOrHorizontalMove {

    for (int i = 0; i < white.size(); ++i) {
      if (white.get(i).getX() % 2 == 1 && white.get(i).getY() % 2 == 0
          || white.get(i).getX() % 2 == 0 && white.get(i).getY() % 2 == 1) {
        throw new VerticalOrHorizontalMove("White made illegal move\n");
      }
    }

    for (int i = 0; i < black.size(); ++i) {
      if (black.get(i).getX() % 2 == 1 && black.get(i).getY() % 2 == 0
          || black.get(i).getX() % 2 == 0 && black.get(i).getY() % 2 == 1) {
        throw new VerticalOrHorizontalMove("Black made illegal move\n");
      }
    }
  }

  private class PiecePosition {
    public PiecePosition(Integer x, Integer y, Piece affiliation) {
      this.x = x;
      this.y = y;
      this.affiliation = affiliation;
    }

    public Integer getX() {
      return x;
    }

    public Integer getY() {
      return y;
    }

    public Piece getAffiliation() {
      return affiliation;
    }

    public void setX(Integer x) {
      this.x = x;
    }

    public void setY(Integer y) {
      this.y = y;
    }

    public void setAffiliation(Piece affiliation) {
      this.affiliation = affiliation;
    }

    private Integer x;
    private Integer y;
    private Piece affiliation;
  }

  private ArrayList<PiecePosition> white;
  private ArrayList<PiecePosition> black;
  private ArrayList<ArrayList<Piece>> board;
}