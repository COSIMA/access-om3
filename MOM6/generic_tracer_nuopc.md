# Coupled generic_tracers with NUOPC-coupled MOM6

NOAA-GFDL maintain a number of modules for modelling tracers, implemented via the GFDL “generic_tracer” API. These modules are “generic” in the sense that they can be used by both MOM and GOLD. They include a number of BGC models (BLING, COBALT, ERGOM, TOPAZ, miniBLING) and other useful tracers (e.g. CFC, SF6). The modules can be found [here](https://github.com/NOAA-GFDL/ocean_BGC/tree/master/generic_tracers).

Many of the GFDL generic_tracer modules require coupling with other components of the earth system. For example, BLING carries a number of tracers with associated air-sea gas fluxes, runoff fluxes or wet/dry deposition fluxes. Importantly, the additional fluxes required—and associated coupled fields—depends on which/how generic_tracers have been configured for use *at runtime*.

In the generic_tracer modules, coupling of these additional tracer fluxes is handled via [FMS coupler_types](https://github.com/NOAA-GFDL/FMS/blob/main/coupler/coupler_types.F90) which are designed for use with the [FMScoupler](https://github.com/NOAA-GFDL/FMScoupler). Modifications to ACCESS-OM3 (which uses CMEPS/NUOPC for coupling, not FMScoupler) are required to allow the use of coupled generic_tracer modules. These modifications are described here.

# Background

## FMS coupler types

In [FMScoupler](https://github.com/NOAA-GFDL/FMScoupler), the handling of tracer fluxes in a fully-coupled system revolves around three related FMS [`coupler_1d_bc_type`](https://github.com/NOAA-GFDL/FMS/blob/38bfde30e1cb8bf5222410a9c37e71529567bf69/coupler/coupler_types.F90#L365-L372) data structures in [`FMScoupler:flux_exchange_mod`](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/flux_exchange.F90#L594-L605):

- `ex_gas_fields_atm`: for atmospheric surface fields associated with tracer fluxes and related parameters
- `ex_gas_fields_ice`: for ice top and ocean surface fields associated with tracer fluxes and related parameters
- `ex_gas_fluxes`: for tracer fluxes and related fields

Together, these structures define the additional tracer fluxes required, the fields needed for their calculation, and the calculation method/parameters to use (defined by the flux type and implementation). A number of flux types and implementations can be used and are described in the `/coupler_mod/types/` field manager list (see [here](https://github.com/NOAA-GFDL/FMS/blob/38bfde30e1cb8bf5222410a9c37e71529567bf69/coupler/atmos_ocean_fluxes.F90#L937) for where this list is defined). There are multiple implementations (i.e. calculation methods) for air-sea gas, air-sea deposition and land-sea runoff flux types. Each flux in the `ex_gas_*` structures has a type and implementation and is associated with an entry in the `/coupler_mod/fluxes/` field manager list.

As an example, suppose we require an additional tracer flux `"co2_flux"` of type `"air_sea_gas_flux_generic"` and implementation `"ocmip2"` (`”ocmip2”` describes a particular method for calculating air-sea gas fluxes). After initialising the `ex_gas_*` structures with this flux, the relevant entry in the `/coupler_mod/types/` field manager list is:
```bash
air_sea_gas_flux_generic/
      implementation/
            ocmip2/
                  num_parameters = 2
            duce/
                  num_parameters = 1
            johnson/
                  num_parameters = 2
      num_flags = 0
      use_atm_pressure = T
      use_10m_wind_speed = T
      pass_through_ice = F
      atm/
            name[1] = 'pcair'
            name[2] = 'u10'
            name[3] = 'psurf'
            long_name[1] = 'Atmospheric concentration'
            long_name[2] = 'Wind speed at 10 m'
            long_name[3] = 'Surface atmospheric pressure'
            units[1] = 'mol/mol'
            units[2] = 'm/s'
            units[3] = 'Pa'
      ice/
            name[1] = 'alpha'
            name[2] = 'csurf'
            name[3] = 'sc_no'
            long_name[1] = 'Solubility w.r.t. atmosphere'
            long_name[2] = 'Ocean concentration'
            long_name[3] = 'Schmidt number'
            units[1] = 'mol/m^3/atm'
            units[2] = 'mol/m^3'
            units[3] = 'dimensionless'
      flux/
            name[1] = 'flux'
            name[2] = 'deltap'
            name[3] = 'kw'
            name[4] = 'flux0'
            long_name[1] = 'Surface flux'
            long_name[2] = 'Ocean-air delta pressure'
            long_name[3] = 'Piston velocity'
            long_name[4] = 'Surface flux no atm'
            units[1] = 'mol/m^2/s'
            units[2] = 'uatm'
            units[3] = 'm/s'
            units[4] = 'mol/m^2/s'
```

the `"co2_flux"` entry in the `/coupler_mod/fluxes/` field manager list is something like:

```bash
co2_flux/
      flux_type = 'air_sea_gas_flux_generic'
      implementation = 'ocmip2'
      atm_tr_index = 0
      mol_wt = 44.0099500000000
      ice_restart_file = 'ice_bling.res.nc'
      ocean_restart_file = 'ocean_bling_airsea_flux.res.nc'
      param[1] = 9.360000000000000E-007
      param[2] = 9.756100000000000E-006
      flag = NULL
      flux-units = 'mol/m^2/s'
      flux-long_name = 'Surface flux'
      deltap-units = 'uatm'
      deltap-long_name = 'Ocean-air delta pressure'
      kw-units = 'm/s'
      kw-long_name = 'Piston velocity'
      flux0-units = 'mol/m^2/s'
      flux0-long_name = 'Surface flux no atm'
      pcair-units = 'mol/mol'
      pcair-long_name = 'Atmospheric concentration'
      u10-units = 'm/s'
      u10-long_name = 'Wind speed at 10 m'
      psurf-units = 'Pa'
      psurf-long_name = 'Surface atmospheric pressure'
      alpha-units = 'mol/m^3/atm'
      alpha-long_name = 'Solubility w.r.t. atmosphere'
      csurf-units = 'mol/m^3'
      csurf-long_name = 'Ocean concentration'
      sc_no-units = 'dimensionless'
      sc_no-long_name = 'Schmidt number'
```

and (assuming `ind_co2` is the index for the `"co2_flux"` flux):
- `ex_gas_fields_atm%bc(ind_co2)` is initialised to carry the fields `pcair`, `u10` and `psurf`
- `ex_gas_fields_ice%bc(ind_co2)` is initialised to carry the fields `alpha`, `csurf` and `sc_no`
- `ex_gas_fluxes%bc(ind_co2)` is initialised to carry the fields `flux`, `deltap`, `kw` and `flux0`

During a coupling loop with FMScoupler the fields for each flux in `ex_gas_fields_atm` and `ex_gas_fields_ice` are set and the fluxes in `ex_gas_fluxes` are calculated from these fields. **Importantly, the arrays in the `ex_gas_*` structures are 1-dimensional (`coupler_1d_bc_type`). They will store the fields on the exchange grid used in FMScoupler.**

## Additional tracer flux handling

A (greatly) simplified summary of the handling of additional tracer fluxes in FMScoupler is as follows:

- Initialise `ex_gas_fields_atm`, `ex_gas_fields_ice` and `ex_gas_fluxes` for the additional tracer fluxes. This step calls initialisation routines within the ocean and atmosphere models to determine what additional tracer fluxes are required. No field arrays set at this point. (via [`gas_exchange_init`](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/coupler_main.F90#L1689)).
- Spawn a 2D version, `Ocean%fields` (`coupler_2d_bc_type`), of `ex_gas_fields_ice`. (via [`ocean_model_init`](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/coupler_main.F90#L1801))
- Calculate/set field arrays for `alpha`, `csurf` and `sc_no` for air-sea gasfluxes in `Ocean%fields`. (via [`ocean_model_init`](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/coupler_main.F90#L1801))
- Spawn a 2D version, `Atm%fields`, of `ex_gas_fields_atm`. (via [`flux_exchange_init`](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/coupler_main.F90#L1857))
- Spawn a 2D version, `Ice_ocean_boundary%fluxes`, of `ex_gas_fluxes`. (via [`flux_exchange_init`](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/coupler_main.F90#L1857))
- **>>> Begin coupling loop**
- Set arrays for `pcair` (air-sea gas fluxes) and `deposition` (air-sea deposition fluxes) in `Atm%fields`. (via [`atmos_tracer_driver_gather_data`](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/coupler_main.F90#L789))
- Set arrays for `u10` and `psurf` for air-sea gas fluxes in `Atm%fields`. (via [`sfc_boundary_layer`](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/coupler_main.F90#L795))
- Map `Ocean%fields` and `Atm%fields` onto exchange grid to set 1D fields in `ex_gas_fields_ice` and `ex_gas_fields_atm`, respectively. (via [`sfc_boundary_layer`](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/coupler_main.F90#L795))
- Calculate fluxes `ex_gas_fluxes` from `ex_gas_fields_ice` and `ex_gas_fields_atm` according to flux types and implementations. (via [`sfc_boundary_layer`](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/coupler_main.F90#L795))
- **Update atmosphere** (this is actually done in multiple steps within a fast coupling loop, but here we’ve simplified)
- Map 1D `ex_gas_fluxes` fields onto 2D `Ice_ocean_boundary%fluxes` (via [`flux_down_from_atmos`](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/coupler_main.F90#L857) and [`flux_atmos_to_ocean`](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/coupler_main.F90#L907))
- **Update land** (this is actually done in multiple steps across two coupling loops, but here we’ve simplified)
- **Update ice** (this is actually done in multiple steps across two coupling loops, but here we’ve simplified)
- **Update ocean**, applying `Ice_ocean_boundary%fluxes`. Calculate/set field arrays for `alpha`, `csurf` and `sc_no` for air-sea gas fluxes in `Ocean%fields`. (via [`update_ocean_model`](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/coupler_main.F90#L1063))
- **<<< End coupling loop**

### Schematic summary

The schematic below traces the handling of additional tracer fluxes in more detail and shows where simplifications were made in the above summary. Note that I generated this schematic by reading the source code and it hasn’t (yet) been verified by any FMScoupler developer. To the best of my knowledge it is mostly complete/correct but I make no guarantees.

<img src="https://github.com/dougiesquire/access-om3/assets/42455466/001ba241-1ae8-4bb0-99b1-b63b5569f85b" width=100%>

# NUOPC-coupled MOM6 and generic_tracers

Code changes are required to allow the use of coupled generic_tracers with NUOPC-coupled MOM6. These changes are guided by the following design principles:

- Use exisiting code where possible, with as little change as possible
- Avoid making edits to source code in MOM6 or GFDL generic_tracer modules

The code changes are limited to the [MOM6 NUOPC cap](https://github.com/NCAR/MOM6/tree/d363034fcc99eef960889b613b4144df8d8eea5a/config_src/drivers/nuopc_cap) and broadly do as follows:

1. Initialisation phases
    1. Initialise `coupler_1d_bc_type` data structures `ex_gas_fields_atm`, `ex_gas_fields_ocn` and `ex_gas_fluxes`. Note these structures are never actually populated with data (unlike in FMScoupler) - they are just used to spawn 2D structures (`coupler_2d_bc_type`).
    2. Initialise ocean model with (a pointer to) `ex_gas_fields_ocn` to populate relevant fields.
    3. Spawn 2D `Ice_ocean_boundary%fluxes` from (a pointer to) `ex_gas_fluxes`.
    4. Spawn 2D `atm_fields` from (a pointer to) `ex_gas_fields_atm`.
    5. Using (a pointer to) `ex_gas_fluxes`, register with NUOPC the additional atmospheric import fields required flux calculations. Field export has not yet been implemented.
2. Advance phase
    1. Get/set atmospheric fields in `atm_fields` from the coupler.
    2. Calculate the fluxes in `Ice_ocean_boundary%fluxes` using a modified version of the routine used by FMScoupler that operates on FMS `coupler_2d_bc_type` inputs.

The additional fluxes will be applied from `Ice_ocean_boundary%fluxes` when the model is advanced.

## Code structure

The most important changes made include modifications to:

- `MOM6/config_src/drivers/nuopc_cap/mom_cap.F90`
- `MOM6/config_src/drivers/nuopc_cap/mom_cap_methods.F90`

and the addition of a new module to the NUOPC cap: `mom_cap_gtracer_flux.F90`

Code modifications/additions have been made via patch files found in the `patches` directory. The new module can be found in the `extra_sources` directory.

### The `mom_cap_gtracer_flux` module

This is where most of the new code is, with many of the changes to `mom_cap.F90` and `mom_cap_methods.F90` being calls to routines defined here. This module also includes the data structures `ex_gas_fields_atm`, `ex_gas_fields_ocn` and `ex_gas_fluxes`. Public routines are:

- `gas_exchange_init`: initialise `ex_gas_fields_atm`, `ex_gas_fields_ocn` and `ex_gas_fluxes` and optionally returns pointers to them.
- `gas_fields_restore`: restore an FMS `coupler_2d_bc_type` state from the ocean restart file defined internally.
- `gas_fields_restart`: write restart for an FMS `coupler_2d_bc_type` to the ocean restart file defined internally.
- `add_gas_fluxes_param`: for each flux in an FMS `coupler_2d_bc_type`, retrieve the `param` array from the `/coupler_mod/fluxes/` field manager list and set it in the `coupler_2d_bc_type`. This is only needed because spawning a `coupler_*d_bc_type` does not copy the `param` array into the spawned type.
- `get_coupled_field_name`: provides the CMEPS field standard name of any fields that need to be coupled for a given generic_tracer flux name. Currently only generic_tracer fluxes `"co2_flux"` and `"o2_flux"` have been implemented.
- `atmos_ocean_fluxes_calc`: calculates the tracer fluxes according to their flux type and implementation. This routine was copied from [FMScoupler](https://github.com/NOAA-GFDL/FMScoupler/blob/6442d387153064644325c96a5e9e2935139d5e3c/full/atmos_ocean_fluxes_calc.F90) and modified. Key modifications include:
    - Operate on `coupler_2d_bc_type` inputs, rather than `coupler_1d_bc_type`.
    - Include calculation for `"air_sea_deposition"` flux types (which in FMScoupler is done in a separate step)
    - Account for sea-ice fraction (in FMScoupler this is done as part of the exchange grid mapping)
    - Make `tsurf` input optional, as it is only used by a few implementations

### Schematic summary

The schematic below traces the handling of coupled generic_tracer fluxes in the MOM6 NUOPC cap.

<img src="https://github.com/dougiesquire/access-om3/assets/42455466/b39aff05-4193-42e9-83f7-cce9cb7a974f" width=65%>

## Diagnostics

Diagnostics can be output for the FMS `coupler_2d_bc_type` fields involved in the handling of the tracer fluxes (flux fields: `Ice_ocean_boundary%fluxes`, ocean fields: `ocean_public%fields`, atmos fields: `atm_fields`). The “model_name” for each type is: flux fields: `“ocean_flux”`, ocean fields: `“ocean_sfc”`, atmos fields: `“atmos_sfc”`. The naming convention for diagnostics of FMS `coupler_bc_type`s is: `<flux_name>_<field_name>_<suffix>`, where `<suffix>` is `”_ice_ocn”`, `”_ocn”` and `”_atm”` for the flux, ocean and atmos fields, respectively. For example, the surface flux field for the `”co2_flux”` example above is called `”co2_flux_flux_ice_ocn”`.

The flux and atmos diagnostics are sent immediately after the tracer flux calculation is done, prior to advancing the model. The ocean diagnostics are sent after advancing the model. This means that the flux/atmos diagnostics are not available at Tfinal+dt (whereas the ocean diagnostics are) and the ocean diagnostics are not available at Tstart (whereas the flux/atmos diagnostics are).

## Data override

FMS data override functionality has been added to allow the tracer fluxes and the contributing atmospheric fields to be overridden via a `data_table` using the component name `"OCN"`. The naming convention for overriding fields is as described above. E.g. one could override the atmospheric concentration field for the `”co2_flux”` example above using the fieldname `”co2_flux_pcair_atm”`
