"""
Parse Strava files from .gpx to a long csv file. Also extracts metadata.

Usage: python3 parse_strava_activities.py <path to Strava export dir> -y <year of interest> [--no-convert]
"""

import os
import gpxpy
import pandas as pd
import datetime as dt
import pytz
import gzip
import re
import argparse
from fit2gpx import StravaConverter
import warnings


def get_cli_arguments():
    """
    Parse the command line arguments
    """

    parser = argparse.ArgumentParser()

    parser.add_argument(
        dest="directory",
        help="The path to the Strava activity files",
    )
    parser.add_argument(
        "-y",
        "--year",
        dest="year",
        type=int,
        default=dt.date.today().year - 1,
        help="The year for which to parse the activities",
    )
    parser.add_argument(
        "--no-convert",
        dest="convert_files",
        default=True,
        action="store_false",
        help="Don't convert the FIT files to GPX",
    )
    parser.add_argument(
        "-q",
        "--quiet",
        dest="quiet",
        default=False,
        action="store_true",
        help="Set output to quiet",
    )

    args = parser.parse_args()

    return args


def _prepare_fit_files(directory, quiet=False):
    """
    Prepare any .fit.gz files and convert to .gpx
    """

    print("Converting FIT files to GPX")

    with warnings.catch_warnings(action="ignore"):
        strava_conv = StravaConverter(dir_in=directory)
        strava_conv.unzip_activities()
        strava_conv.strava_fit_to_gpx()


def get_list_of_activities(directory, year):
    """
    Get a list of all the .gpx files for each activity
    """

    df_activities = pd.read_csv(os.path.join(directory, "activities.csv"))
    df_activities.columns = df_activities.columns.map(
        lambda x: x.lower().replace(" ", "_")
    )

    relevant_files = df_activities.loc[
        df_activities["activity_date"].str.contains(str(year)), "activity_id"
    ].to_list()

    gpx_files = os.listdir(os.path.join(directory, "activities"))
    gpx_files = [x for x in gpx_files if int(os.path.splitext(x)[0]) in relevant_files]
    gpx_files = [os.path.join("activities", x) for x in gpx_files if "fit" not in x]

    fit_files = os.listdir(os.path.join(directory, "activities_gpx"))
    fit_files = [x for x in fit_files if int(os.path.splitext(x)[0]) in relevant_files]
    fit_files = [os.path.join("activities_gpx", x) for x in fit_files]

    all_files = fit_files + gpx_files

    return all_files


def _parse_gpx(filepath, year, quiet=False):
    """
    Parse a single .gpx file, only if date is in prespecified year
    """

    if filepath.endswith(".gz"):
        with gzip.open(filepath, "r") as gpx_filepath:
            gpx = gpxpy.parse(gpx_filepath)
    else:
        with open(filepath, "r") as gpx_filepath:
            gpx = gpxpy.parse(gpx_filepath)

    try:
        if gpx.time:
            gpx_date = dt.datetime.date(gpx.time)
        else:
            gpx_date = dt.datetime.date(gpx.tracks[0].segments[0].points[0].time)
    except IndexError:
        print(f"Skipping {filepath} due to missing date")
        return

    activity_name = gpx.to_xml()
    activity_name = re.search("<name>(.*?)</name>", activity_name)
    if activity_name:
        activity_name = activity_name.group(1)
    else:
        activity_name = None

    activity_duration = gpx.get_duration()
    activity_duration = str(dt.timedelta(seconds=activity_duration))

    doi_begin = dt.datetime.date(dt.datetime.strptime(f"{year}-01-01", "%Y-%m-%d"))
    doi_end = dt.datetime.date(dt.datetime.strptime(f"{year+1}-01-01", "%Y-%m-%d"))

    if (gpx_date >= doi_begin) & (gpx_date < doi_end):
        if not quiet:
            print(f"Parsing data from {filepath}")

        moving_data = gpx.get_moving_data()

        df = pd.DataFrame()
        for track in gpx.tracks:
            for segment in track.segments:
                for point in segment.points:
                    point_df = pd.DataFrame(
                        {
                            "activity_id": os.path.basename(filepath).split(".")[0],
                            "name": activity_name,
                            "date": dt.datetime.date(point.time),
                            "time": dt.datetime.strftime(
                                point.time.astimezone(pytz.timezone("Europe/Oslo")),
                                "%H:%M%:%S",
                            ),
                            "latitude": point.latitude,
                            "longitude": point.longitude,
                            "elevation": point.elevation,
                            "moving_distance": moving_data.moving_distance,
                            "stopped_distance": moving_data.stopped_distance,
                            "total_duration": activity_duration,
                            "moving_time": moving_data.moving_time,
                            "stopped_time": moving_data.stopped_time,
                        },
                        index=[0],
                    )
                    df = pd.concat([df, point_df]).reset_index(drop=True)
                    point_df = pd.DataFrame()
    else:
        if not quiet:
            print("Date is outside of desired date range")
            df = pd.DataFrame()

    return df


def _combine_activities(df_main, df_activity):
    """
    Combine the parsed .gpx files into a single data frame
    """
    df_main = pd.concat([df_main, df_activity]).reset_index(drop=True)
    return df_main


def collect_activities(directory, year, convert_files=True, quiet=False):
    """
    Combine all the activities into a single data frame
    """

    if convert_files:
        _prepare_fit_files(directory)

    list_of_files = get_list_of_activities(directory, year)

    if not quiet:
        print(f"\nParsing activities from year: {year}\n")

    df_collected = pd.DataFrame()
    for i, f in enumerate(list_of_files):
        if not quiet:
            print(f"Parsing year {year}: {f} ({i/len(list_of_files):.2%})")
        df = _parse_gpx(filepath=os.path.join(directory, f), year=year, quiet=quiet)
        df_collected = _combine_activities(df_collected, df)

    return df_collected


def write_to_file(df, outpath, quiet=False):
    """
    Write the final data frame to a file
    """

    if not quiet:
        print(
            f'\nWriting {df["activity_id"].nunique()} activities to file: {outpath}\n'
        )

    df.to_parquet(outpath, index=False)


def main():
    args = get_cli_arguments()

    df = collect_activities(
        directory=args.directory,
        year=args.year,
        convert_files=args.convert_files,
        quiet=args.quiet,
    )
    write_to_file(df, f"./data/strava_data_{args.year}.parquet", quiet=args.quiet)


if __name__ == "__main__":
    main()
