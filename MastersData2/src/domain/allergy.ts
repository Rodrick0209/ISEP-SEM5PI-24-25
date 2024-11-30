import { Entity } from "../core/domain/Entity";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { IAllergyDTO } from "../dto/IAllergyDTO";
import { AggregateRoot } from "../core/domain/AggregateRoot";
import { AllergyCatalogItem } from "./allergyCatalogItem";

interface AllergyProps {
    allergyCatalogItem: AllergyCatalogItem;
    date: Date;
}

export class Allergy extends AggregateRoot<AllergyProps> {
    get id() : UniqueEntityID {
        return this._id;
    }

    get allergyId (): UniqueEntityID {
        return this.id;
    }

    get name (): string {
        return this.props.allergyCatalogItem.name;
    }

    get date (): Date {
        return this.props.date;
    }

    private constructor (props: AllergyProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create (allergyDTO: IAllergyDTO, id?: UniqueEntityID): Result<Allergy> {
        const allergyCatalogItem = allergyDTO.allergyCatalogItem;
        const date = allergyDTO.date;

        if ( allergyCatalogItem === null || date === null) {
            return Result.fail<Allergy>('Must provide a valid allergy and date for the allergy')
        } else {
            const allergy = new Allergy({ allergyCatalogItem: allergyCatalogItem, date: date }, id);
            return Result.ok<Allergy>( allergy )
        }

    }



}