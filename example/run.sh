ggadt_binary=../src/ggadt

$ggadt_binary --parameter-file=parameters_diffscat.ini > diffscat.dat || exit 1
$ggadt_binary --parameter-file=parameters_total_xs.ini > total_xs.dat || exit 1
$ggadt_binary --parameter-file=parameters_diffscat_2d.ini > diffscat_2d.dat || exit 1

python plot_diffscat.py diffscat.dat &
python plot_diffscat_2d.py diffscat_2d.dat &
python plot_total_xs.py total_xs.dat &

echo "Plots should show up; they take a moment."
