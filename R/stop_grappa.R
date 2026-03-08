#' Stop gRappa playback by closing common media players
#'
#' This function tries to stop media playback by closing common
#' media players on Linux, macOS, and Windows.
#'
#' @return Invisibly returns TRUE
#' @export

stop_grappa <- function() {

  sys_name <- Sys.info()[["sysname"]]

  if (sys_name == "Linux") {
    players <- c("vlc", "totem", "mpv", "rhythmbox", "celluloid")

    for (player in players) {
      system2("pkill", args = c("-f", player), stdout = FALSE, stderr = FALSE)
    }

    return(invisible(TRUE))
  }

  if (sys_name == "Darwin") {
    apps <- c("VLC", "QuickTime Player", "IINA", "Elmedia Player")

    for (app in apps) {
      system2("osascript",
              args = c("-e", sprintf('tell application "%s" to quit', app)),
              stdout = FALSE, stderr = FALSE)
    }

    return(invisible(TRUE))
  }

  if (sys_name == "Windows") {
    processes <- c(
      "vlc.exe",
      "wmplayer.exe",
      "Video.UI.exe",
      "Microsoft.Media.Player.exe",
      "PotPlayerMini64.exe",
      "PotPlayerMini.exe"
    )

    for (proc in processes) {
      shell(sprintf('taskkill /IM %s /F', proc), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }

    return(invisible(TRUE))
  }

  warning("Unsupported operating system.", call. = FALSE)
  invisible(FALSE)
}
