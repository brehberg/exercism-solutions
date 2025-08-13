using System;

abstract class Character
{
    private string _type;
    protected Character(string characterType) => this._type = characterType;
    public abstract int DamagePoints(Character target);
    // Make characters not vulnerable by default
    public virtual bool Vulnerable() => false;
    // Describe a character
    public override string ToString() => $"Character is a {this._type}";
}

class Warrior : Character
{
    public Warrior() : base("Warrior") { }
    // Calculate the damage points for a Warrior
    public override int DamagePoints(Character target) => (target.Vulnerable()) ? 10 : 6;
}

class Wizard : Character
{
    private bool _prepared = false;
    public Wizard() : base("Wizard") { }
    // Calculate the damage points for a Wizard
    public override int DamagePoints(Character target) => (this._prepared) ? 12 : 3;
    // Make Wizards vulnerable when not having prepared a spell
    public override bool Vulnerable() => !this._prepared;
    // Allow Wizards to prepare a spell
    public void PrepareSpell() => this._prepared = true;
}
