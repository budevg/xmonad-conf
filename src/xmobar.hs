Config
  { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
  , bgColor = "black"
  , fgColor = "grey"
  , position = TopW L 90
  , commands = [ Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
               , Run Memory ["-t","Mem: <usedratio>%"] 10
               , Run Date "%a %b %_d %H:%M " "date" 10
               , Run Com "/bin/bash" ["-c", "~/.xmonad/scripts/get-volume.sh"] "volume" 10
               , Run Battery [ "-t", "<acstatus>: <left>%"
                             , "-L", "25", "-l", "red"
                             , "-H", "80", "-h", "green"
                             , "-n", "yellow"
                             , "--"
                             , "-O", "AC"
                             , "-o", "Bat"
                             ] 10
               , Run StdinReader
               ]
  , sepChar = "%"
  , alignSep = "}{"
  -- , template = "%StdinReader% }{ %cpu% | %memory% | Vol: %volume% | <fc=#ee9a00>%date%</fc>"
  , template = "%StdinReader% }{ %battery% | %cpu% | %memory% | Vol: %volume% | <fc=#ee9a00>%date%</fc>"
  }
