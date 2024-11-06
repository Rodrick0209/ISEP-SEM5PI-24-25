# US 6.4.4 - As an administrator, I want to identify and quantify the risks involved in the recommended solution.

As a Systems Administrator, I want to identify and quantify the risks involved in the recommended solution, based on the project in question.

## 1. How to Assess the Risks Involved

Risk assessments are typically represented by a risk matrix, where each element has two factors: the probability and the estimated impact on the system. The risk measure is given by multiplying these two factors:

**Risk = Impact x Probability**

Probabilities are rated on a scale from 1 (least likely) to 5 (most likely), and impact is rated on a scale from 1 (marginal) to 4 (catastrophic).

![](Risk_Matrix.png)

In our case, the risk matrix will be used as an aid to determine risks associated with potential damage to the system. A table has been created with all identified threats, determining their factors and associated risks:

| Threat                       | Probability        | Impact             | Risk Level |
|------------------------------|--------------------|--------------------|------------|
| System Failure               | 1 (Unlikely)      | 4 (Catastrophic)   | 4 (Medium) |
| Server Failures              | 2 (Remote)        | 3 (Critical)       | 6 (Medium) |
| Authentication Failures (Unauthorized Access) | 3 (Occasional) | 3 (Critical) | 9 (Serious) |
| Database Loss                | 1 (Unlikely)      | 4 (Catastrophic)   | 4 (Medium) |
| Sensitive Data Leak          | 2 (Remote)        | 3 (Critical)       | 6 (Medium) |
| System Vulnerabilities       | 2 (Remote)        | 4 (Catastrophic)   | 8 (Serious) |

Based on the table, it can be confirmed that most threats have a low probability but should always be addressed due to the associated impact.

Risk assessment is a necessary tool in defining Business Continuity Management (BCM), as only with this assessment is it possible to establish preventive measures for each identified risk, ensured by the implementation of this tool.

---
