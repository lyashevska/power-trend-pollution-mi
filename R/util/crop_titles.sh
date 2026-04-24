# remove titles for publication

mkdir -p no_titles

for f in *.png; do
  if [[ "$f" == *_hist.png ]]; then
    chop=20
  else
    chop=120
  fi

  magick "$f" -gravity north -chop 0x${chop} "no_titles/$f"
done