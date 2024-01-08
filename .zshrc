export EDITOR=/usr/bin/nvim

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

ZSH=/usr/share/oh-my-zsh/
ZSH_CUSTOM=/usr/share/zsh

ZSH_THEME="../../zsh-theme-powerlevel10k/powerlevel10k"

plugins=(
	git direnv git-auto-fetch
	gitfast
	fd
	fzf
	zsh-syntax-highlighting
	zsh-autosuggestions

)

ZSH_CACHE_DIR=$HOME/.cache/oh-my-zsh
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

source $ZSH/oh-my-zsh.sh

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Functions
function scale(){
	if [ -z $1 ]; then
		echo "set a dpi"
		return 1
	fi

	factor=$1
	dpi=$((factor * 96))

	if [ ! -f ~/.Xresources ]; then
		echo "Creating .Xresources"
		touch ~/.Xresources
	fi

	if grep -q '^Xft.dpi:' ~/.Xresources; then
		sed -i "s/^Xft.dpi:.*/Xft.dpi: $dpi/" ~/.Xresources
	else
		echo "Xft.dpi: $dpi" >> ~/.Xresources
	fi

	xrdb $HOME/.Xresources
	i3-msg restart

	echo "xft.dpi set to $dpi"
}

function evdev(){
input_devices=$(ls /dev/input/by-id/*event* | grep -v if)

for device in $input_devices
do
if [[ $device == *"kbd"* && $device == *"Keyboard"* ]]; then
echo "<input type='evdev'>
  <source dev='$device' grab='all' repeat='on' grabToggle='ctrl-ctrl'/>
  </input>"
else
echo "<input type='evdev'>
  <source dev='$device'/>
</input>"
fi
done | xclip -sel c
}

function google() {
  local IFS=+
  xdg-open "http://google.com/search?q=${*}"
}

function pkgSync(){
	cd $HOME
	git pull
#	command systemctl restart --user daemon-reload

	local package
	local packages
	local depends
	eval $(sed -n "/#startPackages/,/#endPackages/p" $HOME/config/PKGBUILD | rg -v '#')
	packages=$depends

	local targetPackages
	targetPackages=$(<<< $packages | tr ' ' '\n')
	local originalPackages=$targetPackages


	local newPackages
	newPackages=$(paru -Qeq | rg -xv $(<<< $targetPackages | tr '\n' '|'))

	if [ ! -z $newPackages ]; then
		echo "$(<<< $newPackages | grep -v linux-config | wc -l) new Packages"
		while read -r package; do
			if [[ "$package" == "linux-config" ]]; then
				continue
			fi
			paru -Qi $package
			read -k 1 "choice?[A]dd, [r]emove, [d]epends or [s]kip $package"
			case $choice in;
				[Aa])
					echo "====================="
					echo
					targetPackages="$targetPackages\\n$package"
					paru -D --asdeps $package
					;;
				[Rr])
					echo "====================="
					echo
					paru -R --noconfirm $package
					;;
				[Dd])
					echo "====================="
					echo
					paru -D --asdeps $package
					;;
				*)
					:
					;;
			esac
			echo
			echo "============"
			echo
		done <<<"$newPackages"
	else
		echo "No new Packages"
	fi

	local missingPackages
	missingPackages=$(echo $targetPackages | rg -xv $(paru -Qqd | tr '\n' '|'))

	if [ ! -z $missingPackages ]; then
		echo "$(wc -l <<< $missingPackages) missing Packages"
		while read -r package; do
			if paru -Qi $package >/dev/null; then
				paru -D --asdeps $package
				continue
			fi
			paru -Si $package
			paru -Qi $package
			read -k 1 "choice?[I]nstall, [R]emove $package"
			case $choice in;
				[Ii])
					echo "====================="
					echo
					paru -S --noconfirm $package
					;;
				[Rr])
					echo "====================="
					echo
					targetPackages=$(echo $targetPackages | rg -xv "$packages")
					;;
				*)
					:
					;;
			esac
			echo
			echo "============"
			echo
		done <<<"$missingPackages"
	else
		echo "No missing Packages"
	fi

	local orphanedPackages
	orphanedPackages=$(paru -Qqtd)

	  if [ ! -z $orphanedPackages ]; then
    		echo "$(wc -l <<<$orphanedPackages) orphaned Packages"
    		if read -q "?Remove orphaned packages? "; then
      		while [ ! -z $orphanedPackages ]; do
        		echo "$(wc -l <<<$orphanedPackages) orphaned Packages"
        		paru -R --noconfirm $(tr '\n' ' ' <<<$orphanedPackages)
        		echo
        		orphanedPackages=$(paru -Qqtd)
      		done
    		fi
  		else
    			echo "No orphaned Packages"
  		fi

	newPackages=$( (
      		echo '  #startPackages'
      		echo 'depends=('
      		echo $targetPackages | sort | uniq | sed 's#^#    #g'
      		echo '  )'
      		echo '  #endPackages'
  	) | sed -r 's#$#\\n#g' | tr -d '\n')

	diff <(echo $originalPackages | sort) <(echo $targetPackages | sort)
	
	(cd $HOME/config && makepkg -si --noconfirm)

}

function convertMp4(){
	originalDir="./original"
	if [ ! -d "$originalDir" ]; then
		echo "creating original directory"
		mkdir $originalDir
	fi
	for video in *.mp4; do
		noExt=${video%.mp4}
		ffmpeg -i $video -acodec pcm_s16le -vcodec copy "${noExt}.mov"
		mv "$video" "$originalDir"
	done
	echo $noExt
}

alias ga="git add"
alias gs="git status"
alias gco="git checkout"
alias gb="git branch -a"
alias gl="git log --graph --pretty=oneline --abbrev-commit"
alias gc="git commit -m"
alias gcd="git commit -m '$(date)'"
alias gp="git push"
alias backup="pushd ~/; ga -u; gcd; gp; popd"
alias vim="nvim"
alias vi="nvim"
alias update="pushd ~/config; PACMAN='paru' PACMAN_AUTH='eval' makepkg -fsi"
alias edit="vim ~/config/PKGBUILD"
alias sd="backup; shutdown now"
alias rb="backup; reboot"
alias chrome="google-chrome-stable"
alias ra="ranger"
