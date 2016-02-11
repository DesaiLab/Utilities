PRO refresh,win=win
  IF NOT keyword_set(win) THEN win = !D.window
  wshow,win,/iconic
  wshow,win,iconic=0
END
