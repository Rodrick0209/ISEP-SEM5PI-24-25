import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { MedicalRecordId } from "./medicalRecordId";
import { MedicalCondition } from "./medicalCondition";
import { Allergy} from "./allergy";
import  IMedicalRecordDTO  from "../dto/IMedicalRecordDTO";



interface MedicalRecordProps {
    patientId: string;
    allergies: Allergy[];
    medicalConditions: MedicalCondition[];
  }

export class MedicalRecord extends AggregateRoot<MedicalRecordProps> {
    get id(): UniqueEntityID {
        return this._id;
    }

    get MedicalRecordId (): MedicalRecordId {
        return new MedicalRecordId(this.id.toValue());
    }

    get patientId (): string {
        return this.props.patientId;
    }

    get allergies (): Allergy[] {
        return this.props.allergies;
    }

    set allergies(allergies: Allergy[]) {
        this.props.allergies = allergies;
    }

    set medicalConditions(medicalConditions: MedicalCondition[]) {
        this.props.medicalConditions = medicalConditions;
    }

    get medicalConditions (): MedicalCondition[] {
        return this.props.medicalConditions;
    }

    private constructor (props: MedicalRecordProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create (IMedicalRecordDTO: IMedicalRecordDTO, allergies: Allergy[] ,medicalConditions : MedicalCondition[],id?: UniqueEntityID):  Result<MedicalRecord> {
        const patientId = IMedicalRecordDTO.patientId;

        const medicalRecord = new MedicalRecord(
            { patientId: patientId, 
                allergies: allergies, 
                medicalConditions: medicalConditions }
                ,id);
        return Result.ok<MedicalRecord>( medicalRecord )
    }




}
