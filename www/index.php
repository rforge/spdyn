
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->



<p> The <b>spdyn package</b>, currently under development, seeks to implement a set of methods to perform exploratory
 data analysis on space-time data. Currently, it only implements functions to caculate <i>Spatial Markov Matrices</i>
 as suggested by Rey (2001), and a set of related measures, as the mean first passage times and the estimation of the 
 ergodic distribution of the Markov processes implied by the Spatial Markov Matrices. Further developments will include
 the implemantation of statistical tests to asses if the Spatial Markov Matrices really differ from a non-spatially 
 conditioned Markov Matrix, or classic Markov process.
 Also, it will implement other measures of space-time dynamics exposed by Rey (2001) and the dynamic LISA measures 
 proposed by Rey, Murray and Anselin (2011). </p>

<p><strong><i>References</i></strong></p>
<p>Rey, S.J. (2001) 'Spatial Empirics for Economic Growth and Convergence', Geographical Analysis, Vol.33, No.3.</p>
<p>Rey, S.J., A.T. Murray and L. Anselin. 2011. "Visualizing regional income distribution dynamics." Letters in Spatial and Resource Sciences. Vol. 4.</p>



<p> This package can also be installed locally, by downloading the zip file <a href="https://r-forge.r-project.org/R/?group_id=1908"><strong>here</strong></a>. </p>

</body>
</html>
