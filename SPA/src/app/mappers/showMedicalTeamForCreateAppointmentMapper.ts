import { MedicalTeamShowForAppointmentCreate } from "../models/appointment";


export function mapToMedicalTeamShowForAppointmentCreate(json: any): MedicalTeamShowForAppointmentCreate {
    return {
      staffAnesthesyPhase: json.staffAnesthesyPhase.map((phase: any) => ({
        specializationId: phase.specializationId,
        nrNeededStaff: Number(phase.nrNeededStaff),
        staffId: phase.staffId
      })),
      staffSurgeryPhase: json.staffSurgeryPhase.map((phase: any) => ({
        specializationId: phase.specializationId,
        nrNeededStaff: Number(phase.nrNeededStaff),
        staffId: phase.staffId
      }))
    };
  }