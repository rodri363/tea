
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

<!-- DV: I'm adding a bit more of an introduction as well as a list of what's available on the website and 
where to find it-->
<p> Welcome to the homepage of the TEA for survey processing project. Here, you can find 
anything you need related to TEA, including:
<ul>
	<li>Documentation concerning the usage of TEA in pdf format here: <a href="/tutorial_intro_combined/tutorial_intro_combined.html"><b>TEA Tutorial</b></a></li> <!-- put link to tutorial.pdf-->
	<li>A source from which to download TEA as well as the R packages on which TEA is dependent</li>
	<li>The source code for TEA</li>
	<li>The TEA developers' contact information for any questions or suggestions about the TEA project or the website.</li>
</ul>
</p>


<p> You can find all of the above information at the <strong>project summary page</strong> <a href="https://r-forge.r-project.org/projects/tea/"><strong>here</strong></a>. </p>

</body>
</html>
