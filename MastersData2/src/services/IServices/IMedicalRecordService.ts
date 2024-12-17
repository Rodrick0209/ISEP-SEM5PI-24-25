import { Result } from "../../core/logic/Result";
import IMedicalRecordDTO from "../../dto/IMedicalRecordDTO";


export default interface IMedicalRecordService {
    createMedicalRecord(medicalRecordDTO: IMedicalRecordDTO): Promise<Result<IMedicalRecordDTO>>;
    listMedicalRecords(): Promise<Result<IMedicalRecordDTO[]>>;    
    updateMedicalRecord(MedicalRecordDTO: IMedicalRecordDTO, id : string);
    getMedicalRecordByPatientId(patientId: string): Promise<Result<IMedicalRecordDTO>>;
    searchMedicalRecordEntries(patientId: string, designation: string);
}