import { Component } from '@angular/core';
import { ListAllergiesCatalogComponent } from "../list-allergies-catalog/list-allergies-catalog.component";

@Component({
  selector: 'app-allergies-catalog',
  standalone: true,
  templateUrl: './allergies-catalog.component.html',
  styleUrl: './allergies-catalog.component.css',
  imports: [ListAllergiesCatalogComponent]
})
export class AllergiesCatalogComponent {

}
