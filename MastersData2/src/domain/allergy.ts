import { Entity } from "../core/domain/Entity";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { IAllergyDTO } from "../dto/IAllergyDTO";
import { AggregateRoot } from "../core/domain/AggregateRoot";
import { AllergyCatalogItem } from "./allergyCatalogItem";

interface AllergyProps {
    allergyCatalogItem: AllergyCatalogItem;
    description: string;
}

export class Allergy extends AggregateRoot<AllergyProps> {
    get id() : UniqueEntityID {
        return this._id;
    }

    get allergyId (): UniqueEntityID {
        return this.id;
    }

    get code (): string {
        return this.props.allergyCatalogItem.code;
    }

    get designation (): string {
        return this.props.allergyCatalogItem.designation;
    }
    
    get description (): string {
        return this.props.description;
    }

    private constructor (props: AllergyProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create (allergyDTO: AllergyCatalogItem,desc:string, id?: UniqueEntityID): Result<Allergy> {
        const allergyCatalogItem = allergyDTO;
        const description = desc;

        if ( allergyCatalogItem === null || description === null) {
            return Result.fail<Allergy>('Must provide a valid allergy and description for the allergy')
        } else {
            const allergy = new Allergy({ allergyCatalogItem: allergyCatalogItem, description: description }, id);
            return Result.ok<Allergy>( allergy )
        }

    }




}