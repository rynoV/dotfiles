# https://github.com/moby/moby/issues/27988#issuecomment-462809153
echo 'debconf debconf/frontend select Noninteractive' | sudo debconf-set-selections
sudo apt-get -y -q install --no-install-recommends fzf

# Replace home folder occurences with current home folder
# Need to use @ separator because HOME might contain forward slashes
find . -type f -not -path '.git' -exec sed -i "s@/home/calum@$HOME@g" {} +

rm -rf ~/.config
cp -r ./.config ~/.config
cp ./.gitconfig ~/.gitconfig
cp ./.ghci ~/.ghci

.scripts/fzf/install --xdg --bin
