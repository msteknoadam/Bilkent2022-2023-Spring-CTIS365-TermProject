import * as path from "path";
import * as fs from "fs";
import fetch from "node-fetch";
import { parse } from "node-html-parser";

const TARGET_DEPTS = ["CTIS", "CS", "MATH", "LAW", "ECON"];

const DAY_IDX = ["mon", "tue", "wed", "thu", "fri", "sat", "sun"] as const;
const START_HOUR_IDX = [
	"0830",
	"0930",
	"1030",
	"1130",
	"1230",
	"1330",
	"1430",
	"1530",
	"1630",
	"1730",
	"1830",
	"1930",
	"2030",
	"2130",
] as const;

type SectionFiltered = { gpa?: number; schedule: string[] };
type SectionUnfiltered = { instructor: string; schedule: Record<string, string> };
type CourseFiltered = { name: string; gpa?: number; sections: Record<string, SectionFiltered> };
type CourseUnfiltered = { name: string; sections: Record<string, SectionUnfiltered> };
type DeptFiltered = Record<string, CourseFiltered>;
type DeptUnfiltered = Record<string, CourseUnfiltered>;
type OfferingsFiltered = Record<string, DeptFiltered>;
type OfferingsUnfiltered = Record<string, DeptUnfiltered>;

let fetched = false;

async function run() {
	const filteredOfferings: OfferingsFiltered = {};
	const files = [] as string[];

	[2015, 2016, 2017, 2018, 2019, 2021].forEach((year) => {
		[1, 2].forEach((semester) => {
			files.push(`${year}${semester}.json`);
		});
	});

	for (const fileName of files) {
		const currSemester = fileName.replace(".json", "");
		const unfilteredOfferings = (await require(path.join(__dirname, fileName))) as OfferingsUnfiltered;

		for (const dept in unfilteredOfferings) {
			if (TARGET_DEPTS.includes(dept)) {
				const filteredDept: DeptFiltered = {};
				filteredOfferings[dept] = filteredDept;

				for (const courseCode in unfilteredOfferings[dept]) {
					const currUnfilteredCourse = unfilteredOfferings[dept][courseCode];
					const currFilteredCourse: CourseFiltered = { name: currUnfilteredCourse.name, sections: {} };

					Object.entries(currUnfilteredCourse.sections).forEach(([section, details]) => {
						if (Object.keys(details.schedule).length === 0) return;
						currFilteredCourse.sections[section] = {
							schedule: [],
						};
						currFilteredCourse.sections[section].schedule = Object.entries(details.schedule).map(
							([hourIdx, classRoom]) => {
								const hour = +hourIdx;
								const day = hour % 7;
								const startHour = Math.floor(hour / 7);

								const dayStr = DAY_IDX[day];
								const startHourStr = START_HOUR_IDX[startHour];

								return `${dayStr}_${startHourStr}${classRoom.includes("(S)") ? "_s" : ""}`;
							}
						);
					});

					Object.keys(currFilteredCourse.sections).forEach((sec) => {
						if (
							currFilteredCourse.sections[sec].schedule.length !== 4 ||
							currFilteredCourse.sections[sec].schedule.some((d) => d.includes("s"))
						)
							delete currFilteredCourse.sections[sec];
					});

					if (Object.keys(currFilteredCourse.sections).length !== 0) {
						filteredOfferings[dept][courseCode] = currFilteredCourse;
					}
				}

				if (1 > 0 || !fetched) {
					const resp = await fetch(
						`https://stars.bilkent.edu.tr/homepage/ajax/plainOfferings.php?COURSE_CODE=${dept}&SEMESTER=${currSemester}&submit=List%20Selected%20Offerings&rndval=${new Date().getTime()}`,
						{
							headers: {
								accept: "*/*",
								"accept-language": "en-US,en;q=0.9",
								"cache-control": "no-cache",
								pragma: "no-cache",
								"sec-ch-ua": '"Chromium";v="112", "Not_A Brand";v="24", "Opera GX";v="98"',
								"sec-ch-ua-mobile": "?0",
								"sec-ch-ua-platform": '"macOS"',
								"sec-fetch-dest": "empty",
								"sec-fetch-mode": "cors",
								"sec-fetch-site": "same-origin",
								cookie: "authchallenge=fdcdfc9d7717bccd266e603c4d4a62d2; PHPSESSID=5bphf2bgl66sgdelk9sl7as2vn", // Change PHPSESSID to fetch data
								Referer: "https://stars.bilkent.edu.tr/homepage/plain_offerings",
								"Referrer-Policy": "strict-origin-when-cross-origin",
							},
							body: undefined,
							method: "GET",
						}
					);

					const textResp = await resp.text();

					fs.writeFileSync(path.join(__dirname, fileName.replace(".json", "offerings.html")), textResp);

					const parsed = parse(textResp);

					Object.keys(filteredOfferings[dept]).forEach((courseCode) => {
						Object.keys(filteredOfferings[dept][courseCode].sections).forEach((section) => {
							const courseRow1 = parsed.querySelector(`[id="${courseCode}-${section.padStart(3, "0")}"]`);
							const courseRow2 = parsed.querySelector(`[id="${courseCode}-${section}"]`);
							const courseRow = courseRow1 || courseRow2;

							if (!courseRow) {
								console.error(
									`Couldn't find row for ${courseCode}-${section.padStart(3, "0")} on ${currSemester}`
								);
							} else {
								const tdEls = courseRow.querySelectorAll("td");
								const sectionGPAEl = tdEls[8];
								const courseGPAEl = tdEls[9];

								if (sectionGPAEl) {
									filteredOfferings[dept][courseCode].sections[section].gpa = +sectionGPAEl.innerText;
								} else {
									console.error(
										`Couldn't find section GPA for ${courseCode}-${section.padStart(
											3,
											"0"
										)} on ${currSemester}`
									);
								}
								if (courseGPAEl) {
									filteredOfferings[dept][courseCode].gpa = +courseGPAEl.innerText;
								} else {
									console.error(
										`Couldn't find course GPA for ${courseCode}-${section.padStart(
											3,
											"0"
										)} on ${currSemester}`
									);
								}
							}
						});
					});
				}
			}
		}

		const sections = Object.values(filteredOfferings)
			.map((deptCourses) => {
				return Object.entries(deptCourses).map(([courseCode, course]) => {
					return Object.entries(course.sections).map(([sectionId, section]) => {
						return { ...section, courseCode: `${courseCode.replace(" ", "")}_${sectionId}` };
					});
				});
			})
			.flat(2);

		fs.writeFileSync(
			path.join(__dirname, fileName.replace(".json", "sections.json")),
			JSON.stringify(sections, null, 4)
		);
	}
}

run();
