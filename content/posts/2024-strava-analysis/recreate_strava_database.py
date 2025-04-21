"""
Recreate your personal database from the Strava data

Usage: python3 recreate_strava_database.py <path to Strava export dir> <path to SQLite db>
"""

import os
import pandas as pd
import sqlite3
import argparse


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
        dest="db_path",
        help="The path to the SQLite database to store the data in",
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


def _connect_db(path):
    conn = sqlite3.connect(path)

    return conn


def _get_metadata_csv_files(directory):
    """
    Get a list of all the .gpx files for each activity
    """

    csv_files = [
        os.path.join(directory, x) for x in os.listdir(directory) if ".csv" in x
    ]

    return csv_files


def write_metadata_to_database(directory, db_path, quiet=False):
    csv_files = _get_metadata_csv_files(directory)
    conn = _connect_db(db_path)

    for f in csv_files:
        table_name = os.path.basename(f).split(".")[0]
        df_tmp = pd.read_csv(f)
        nrows = df_tmp.to_sql(
            name=table_name, con=conn, index=False, if_exists="replace"
        )
        if not quiet:
            print(f"{nrows} records added to table {table_name}")

    conn.close()


def _get_activity_data(directory, quiet=False):
    indir = "/".join(directory.split("/")[:-2])

    act_files = [os.path.join(indir, x) for x in os.listdir(indir) if ".parquet" in x]

    return act_files


def write_activity_data_to_database(directory, db_path, quiet=False):
    act_files = _get_activity_data(directory)
    conn = _connect_db(db_path)

    act_collect = []
    for f in act_files:
        df_tmp = pd.read_parquet(f)
        act_collect.append(df_tmp)

    df_activity = (
        pd.concat(act_collect).sort_values(["date", "time"]).reset_index(drop=True)
    )

    nrows = df_activity.to_sql(
        name="activity_data", con=conn, index=False, if_exists="replace"
    )
    if not quiet:
        print(f"{nrows} records added to activity_data")


def main():
    args = get_cli_arguments()

    write_metadata_to_database(args.directory, args.db_path)
    write_activity_data_to_database(args.directory, args.db_path)


if __name__ == "__main__":
    main()
