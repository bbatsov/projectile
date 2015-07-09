#!/usr/bin/env python
# -*- coding: utf-8 -*-
""" Bench-Plot

requirements: pip install sexpdata pygal

"""
import pygal
import sexpdata
import os
import fnmatch
import os.path


def read_sexp(filename):
    """
    """
    indata = []
    with open(filename, "r") as myfile:
        text = myfile.read()
        indata = sexpdata.loads(text)

    data = []
    for sexp in indata:
        entry = {
            'file': sexp[0],
            'basedir': sexp[1],
            'profile': sexp[2],
            'test-name': sexp[3].value(),
            'duration': sexp[4],
        }
        data.append(entry)

    return data


def group_by(data, field):
    result = {}
    for entry in data:
        group_name = entry[field]
        group = result.get(group_name, [])
        group.append(entry)
        result[group_name] = group
    return result


def duration(lst):
    duration = 0
    for i in lst:
        duration = duration + i['duration']
    return duration


def plot1(data, filename):
    """
    """
    chart = pygal.StackedBar(
        x_label_rotation=85,
        truncate_label=100,
        truncate_legend=24,
        # label_font_size=8,
        # legend_font_size=8,
        legend_at_bottom=True,
        height=900,
        width=800,
    )
    for i in data:
        i['combined'] = i['profile'] + " " + i['basedir']
    by_test = group_by(data, "test-name")
    by_combined = group_by(data, "combined")
    test_names = sorted(by_test.keys())

    for config_name in by_combined:
        config = by_combined[config_name]
        config_tests = group_by(config, 'test-name')

        durationlist = []
        for test_name in test_names:
            durationlist.append(
                duration(config_tests[test_name]) /
                duration(by_test[test_name]))
            # durationlist.append(duration(config_tests[test_name]))
        chart.add(config_name, durationlist)

    chart.title = 'Time per configuration'
    chart.x_labels = test_names
    chart.render_to_file(filename + ".plot1.svg")


def plot2(data, filename):
    """ this is more or less a copy of the above plot1,
    """
    chart = pygal.Bar(
        x_label_rotation=85,
        truncate_label=100,
        truncate_legend=24,
        legend_at_bottom=True,
        height=900,
        width=800,
        logarithmic=True,
    )
    for i in data:
        i['combined'] = i['profile'] + " " + i['basedir']
    by_test = group_by(data, "test-name")
    by_combined = group_by(data, "combined")
    test_names = sorted(by_test.keys())

    for config_name in by_combined:
        config = by_combined[config_name]
        config_tests = group_by(config, 'test-name')

        durationlist = []
        for test_name in test_names:
            durationlist.append(duration(config_tests[test_name]))
        chart.add(config_name, durationlist)

    chart.title = 'Time per configuration'
    chart.x_labels = test_names
    chart.render_to_file(filename + ".plot2.svg")





def process_files():
    """
    """
    dir = "bench-logs"
    files = fnmatch.filter(os.listdir(dir), "*.txt")
    for file in files:
        data = read_sexp(os.path.join(dir, file))
        data_by_test = group_by(data, "test-name")
        splitdata = group_by(data, 'file')
        for k in splitdata:
            plot2(splitdata[k], os.path.splitext(file)[0] + os.path.basename(k))
            plot1(splitdata[k], os.path.splitext(file)[0] + os.path.basename(k))

if __name__ == '__main__':
    process_files()
