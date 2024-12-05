import { MedicalRecord
 } from "../../domain/medicalRecord";
import { Repo } from "../../core/infra/Repo";
import { MedicalRecordId } from "../../domain/medicalRecordId";


export default interface IMedicalRecordRepo extends Repo<MedicalRecord> {
    save(medicalRecord: MedicalRecord): Promise<MedicalRecord>;
    findAll(): Promise<MedicalRecord[]>;
    removeAll(): Promise<void>;

}