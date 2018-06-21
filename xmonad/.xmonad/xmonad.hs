import XMonad

main :: IO ()
main = do
	xmonad defaultConfig {
		terminal = "gnome-terminal"
	}
