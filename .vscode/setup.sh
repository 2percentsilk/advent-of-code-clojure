# Check java is installed
java -version

# Install lein
sudo apt-get install -y curl
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > lein
sudo mv lein /usr/local/bin/lein
sudo chmod a+x /usr/local/bin/lein
lein version

# Install clj
sudo apt-get install -y bash curl rlwrap
curl -O https://download.clojure.org/install/linux-install-1.10.1.462.sh
chmod +x linux-install-1.10.1.462.sh
sudo ./linux-install-1.10.1.462.sh
rm -rf ./linux-install-1.10.1.462.sh
clj
