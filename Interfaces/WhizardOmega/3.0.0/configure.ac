# Configure the build system for the W2 model files written out by the
# FeynRules WO interface. Most information is directly obtained from the
# whizard-config / omega-config scripts. After the Makefiles have been
# generated, the models contained in the output directory are scanned and
# the model-specific Makefile.src are generated.

# $Id: configure.ac 3435 2011-07-24 22:20:34Z cnspeckn $

AC_INIT([this FeynRules model], [for WHIZARD 2.0])

# The default is to install locally
AC_PREFIX_DEFAULT([${HOME}/.whizard])

# Track down the WHIZARD / O'Mega installation
AC_ARG_VAR([WO_CONFIG], dnl
[path to whizard-config and omega-config (searched _before_ the system-wide
search path)])

# Allow to append compiler flags
AC_ARG_VAR([FCFLAGS], dnl
[override FORTRAN compiler flags])

delimiter="################################################################################"

echo ${delimiter}
AC_MSG_NOTICE([collecting information])
echo ${delimiter}
echo

###############################################################################
# Check for a working install
AC_PROG_INSTALL

###############################################################################
# Look for whizard-config and omega-config
[
wo_confpath=""
test -n "${WO_CONFIG}" && wo_confpath="${WO_CONFIG}:"
wo_confpath="${wo_confpath}${PATH}"
]

# whizard-config ?
AC_PATH_PROG([whizard_config], [whizard-config], [not found], dnl
   [path = ${wo_confpath}])
if test "${whizard_config}" = "not found"; then
   AC_MSG_ERROR([couldn't locate whizard-config: consider setting WO_CONFIG...])
fi

# omega-config ?
AC_PATH_PROG([omega_config], [omega-config], [not found], dnl
   [path = ${wo_confpath}])
if test "${omega_config}" = "not found"; then
   AC_MSG_ERROR([couldn't locate omega-config: consider setting WO_CONFIG...])
fi

###############################################################################
# Query whizard-config and omega-config for the relevant settings

# WHIZARD library path
AC_MSG_CHECKING([for the WHIZARD library path])
WHIZARD_LIBPATH=$(${whizard_config} --libdir)
AC_SUBST([WHIZARD_LIBPATH])
AC_MSG_RESULT([${WHIZARD_LIBPATH}])

# WHIZARD data path
AC_MSG_CHECKING([for the WHIZARD data directory])
WHIZARD_DATAPATH=$(${whizard_config} --datadir)
AC_SUBST([WHIZARD_DATAPATH])
AC_MSG_RESULT([${WHIZARD_DATAPATH}])

# FORTRAN compiler
AC_MSG_CHECKING([for the FORTRAN compiler WHIZARD was built with])
FC=$(${whizard_config} --fc)
AC_SUBST([FC])
AC_MSG_RESULT([${FC}])

# FORTRAN flags
AC_MSG_CHECKING([for the FORTRAN compiler options])
test -z "$FCFLAGS" && FCFLAGS=$(${whizard_config} --fcflags)
AC_SUBST([FCFLAGS])
AC_MSG_RESULT([${FCFLAGS}])

# FORTRAN module extension
AC_MSG_CHECKING([for the file extension used for FORTRAN modules])
FC_MODEXT=$(${whizard_config} --fcmodext)
AC_SUBST([FC_MODEXT])
AC_MSG_RESULT([${FC_MODEXT}])

# libtool
AC_MSG_CHECKING([for the WHIZARD libtool])
LIBTOOL="${WHIZARD_LIBPATH}/libtool"
if test ! -x "${LIBTOOL}"; then
   AC_MSG_RESULT([not found])
   AC_MSG_ERROR(dnl
[${LIBTOOL}
not found or not executable! It seems WHIZARD was built without libtool
support which, however, is mandatory for using FeynRules with W2.])
fi
AC_MSG_RESULT([${LIBTOOL}])
AC_SUBST([LIBTOOL])

# O'Mega library path
AC_MSG_CHECKING([for the O'Mega library path])
OMEGA_LIBPATH=$(${omega_config} --libdir)
AC_SUBST([OMEGA_LIBPATH])
AC_MSG_RESULT([${OMEGA_LIBPATH}])

# ocamlopt
AC_MSG_CHECKING([for ocamlopt (taken from the O'Mega configuration)])
OCAMLOPT=$(${omega_config} --ocamlopt)
if test -z "${OCAMLOPT}"; then
   AC_MSG_RESULT([not found])
   AC_MSG_ERROR(dnl
[It seems O'Mega was built without O'Caml which is mandatory for using
FeynRules!])
fi
AC_MSG_RESULT([${OCAMLOPT}])
AC_SUBST([OCAMLOPT])

# additional ocaml flags
AC_MSG_CHECKING([for any additional O'Caml flags])
OCAMLFLAGS=$(${omega_config} --ocamlflags)
AC_SUBST([OCAMLFLAGS])
screenflags=${OCAMLFLAGS}
test -z "${screenflags}" && screenflags="none"
AC_MSG_RESULT([${screenflags}])

###############################################################################
# Create output files

echo
echo ${delimiter}
AC_MSG_NOTICE([writing Makefiles])
echo $delimiter
echo

AC_CONFIG_FILES([omega/Makefile] [whizard/Makefile] [Makefile])

AC_OUTPUT

###############################################################################
# Generate Makefile includes

echo
echo ${delimiter}
AC_MSG_NOTICE([collecting models])
echo $delimiter
echo

lfeed="
"
for mdl in whizard/*mdl; do
   model=${mdl%.mdl}
   model=${model##*/}
   echo "found: $model"

   # WHIZARD
   libraries="${libraries}\\${lfeed}   libparameters_$model.la"
   models="${models}\\${lfeed}   ${model}.mdl"
   cplsrc="${cplsrc}${lfeed}cplsrc_${model} = "
   for file in whizard/*${model}*cpl*f90; do
      cplsrc="${cplsrc}\\${lfeed}   ${file##*/}"
   done
   cplobj="${cplobj}${lfeed}cplobj = \$(cplsrc_${model}:.f90=.lo)"
   globsrc="${globsrc}${lfeed}globsrc_${model} = parameters.${model}.global.f90"
   globobj="${globobj}${lfeed}globobj_${model} = \$(globsrc_${model}:.f90=.lo)"
   locsrc="${locsrc}${lfeed}locsrc_${model} = parameters.${model}.local.f90"
   locobj="${locobj}${lfeed}locobj_${model} = \$(locsrc_{$model}:.f90=.lo)"
   mainsrc="${mainsrc}${lfeed}mainsrc_${model} = parameters.${model}.f90"
   mainobj="${mainobj}${lfeed}mainobj_${model} = \$(mainsrc_${model}:.f90=.lo)"
   allsrc="${allsrc}${lfeed}src_${model} = \$(globsrc_${model}) \$(locsrc_${model})"
   allsrc="${allsrc} \$(cplsrc_${model}) \$(mainsrc_${model})"
   allobj="${allobj}${lfeed}obj_${model} = \$(src_${model}:.f90=.lo)"
	modules="${modules}\\${lfeed}   \$(patsubst %_f90,%.mod,\$(subst .,_,\$(src_${model})))"
   whizrules="${whizrules}${lfeed}
\$(locobj_${model}): \$(globobj_${model})
\$(cplobj_${model}): \$(globobj_${model}) \$(locobj_${model})
\$(mainobj_${model}): \$(globobj_${model}) \$(locobj_${model}) \$(cplobj_${model})
parameters_${model}.\$(fc_modext): \$(mainobj_${model})
libparameters_$model.la: \$(obj_${model})
	\$(libtool) --tag=FC --mode=link \$(fc) -all-static  \\
	   -rpath \$(prefix)/lib/whizard/models -o \$@ \$(obj_${model})"

   # O'Mega
   binaries="${binaries}\\${lfeed}   omega_${model}.opt"
   oint="${oint}${lfeed}int_${model} = ${model}_mdl.cmi"
   omdl="${omdl}${lfeed}mdl_${model} = ${model}_mdl.cmx"
   obin="${obin}${lfeed}bin_${model} = omega_${model}.cmx"
   orules="${orules}${lfeed}
\$(mdl_${model}): \$(int_${model})
\$(bin_${model}): \$(int_${model}) \$(mdl_${model})
omega_${model}.opt: \$(int_${model}) \$(mdl_${model}) \$(bin_${model})
	\$(ocamlopt) \$(ocamlflags) -I \$(libdir) -o \$@ unix.cmxa omega_core.cmxa omega_targets.cmxa \\
	   \$(mdl_${model}) \$(bin_${model})"
done

echo

AC_MSG_NOTICE([writing whizard/Makefile.src])
cat <<EOI > whizard/Makefile.src || exit 1
models = ${models}
libraries = ${libraries}
modules = ${modules}
${globsrc}${globobj}
${locsrc}${locobj}
${cplsrc}${cplobj}
${mainsrc}${mainobj}
${allsrc}${allobj}
${whizrules}
EOI

AC_MSG_NOTICE([writing omega/Makefile.src])
cat <<EOI > omega/Makefile.src || exit 1
binaries = ${binaries}
${oint}
${omdl}
${obin}
${orules}
EOI

###############################################################################
# Done

cat <<EOI

${delimiter}

Your configure finished, and you're now ready to compile and install the
model via make install.
EOI
